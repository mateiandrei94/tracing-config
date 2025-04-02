//! The config module contains the data model for the configuration file and [`tracing`][mod@tracing] initialization routines.
//!
//! This uses the [`serde`][mod@serde] and [`toml`][mod@toml] crates to serialize and deserialize the configuration file.
//! Once the configuration is read from disk, environment variables are then resolved.
//!
//! See the [`interpolate`][mod@crate::interpolate] module for an understanding of how the variables are resolved.
//!
//! The [`model`][mod@model] submodule simply contains the configuration data model, start from there to understand how to create your tracing.toml.
//!
//! This module contains the tracing initialization routine [`initialize`][fn@initialize].

#[path = "config.model.rs"]
pub mod model;

use serde::{Deserialize, Serialize};

use std::convert::{From, Into};
use std::path::PathBuf;
use std::{fs, io::Write as _, path::Path};
use std::sync::{Arc, Weak, Mutex};

use crate::interpolate::resolve_from_env_recursive;
use crate::tracing::{SpanRecordLayer, JsonLayer, SiftingLayer, SiftingLayerSelector};

use ::tracing_subscriber::layer::SubscriberExt as _;
use ::tracing_subscriber::Layer as _;
use ::tracing_subscriber as ts;
use ::tracing_appender as ta;
use ::tracing as t;

use crate::error::*;

/// println macro when no other framework is available.
macro_rules! emit {
    (TRACE, $verbosity:expr, $($arg:tt)*) => {{
        emit!($verbosity, $crate::config::model::Level::Trace, $($arg)*);
    }};
    (DEBUG, $verbosity:expr, $($arg:tt)*) => {{
        emit!($verbosity, $crate::config::model::Level::Debug, $($arg)*);
    }};
    (INFO, $verbosity:expr, $($arg:tt)*) => {{
        emit!($verbosity, $crate::config::model::Level::Info, $($arg)*);
    }};
    (WARN, $verbosity:expr, $($arg:tt)*) => {{
        emit!($verbosity, $crate::config::model::Level::Warn, $($arg)*);
    }};
    (ERROR, $verbosity:expr, $($arg:tt)*) => {{
        emit!($verbosity, $crate::config::model::Level::Error, $($arg)*);
    }};
    ( $verbosity:expr, $level:expr, $($arg:tt)* ) => {{
        if let Some(verbosity) = $verbosity {
            if verbosity <= $level {
                use ::tracing_subscriber::fmt::time::FormatTime as _;
                let mut now = String::new();
                let mut now_writer = ::tracing_subscriber::fmt::format::Writer::new(&mut now);
                ::tracing_subscriber::fmt::time::time().format_time(&mut now_writer).expect("unable to format_time");
                let module_path = module_path!();
                static ANSI_RESET : &str = "\x1B[0m";
                static ANSI_DIM : &str = "\x1B[2m";
                let (level, ansi_color, ansi_color_bold) = match $level {
                    $crate::config::model::Level::Trace =>("TRACE","\x1B[35m", "\x1B[1;35m",),
                    $crate::config::model::Level::Debug =>("DEBUG","\x1B[34m", "\x1B[1;34m",),
                    $crate::config::model::Level::Info => ("INFO", "\x1B[32m", "\x1B[1;32m",),
                    $crate::config::model::Level::Warn => ("WARN", "\x1B[33m", "\x1B[1;33m",),
                    $crate::config::model::Level::Error =>("ERROR","\x1B[31m", "\x1B[1;31m",),
                };
                println!("{ANSI_DIM}{now}{ANSI_RESET} \
                    {ansi_color}{level}{ANSI_RESET} \
                    {ansi_color_bold}{module_path}{ANSI_RESET}\
                    {ansi_color}: {}{ANSI_RESET}", format!($($arg)*));

            }
        }
    }};
}

static ENV_TRACING_CONFIG: &str = "tracing_config";
static ENV_TRACING_CONFIG_PARENT: &str = "tracing_config_parent";
static ENV_TRACING_CONFIG_TEST: &str = "tracing_config_test";
static ENV_TRACING_CONFIG_TEST_PARENT: &str = "tracing_config_test_parent";

/// The value is set in stone at `25`
///
/// This represents the `depth` with which [`resolve_from_env_recursive`][fn@resolve_from_env_recursive] is called by this module.
pub const RESOLVE_FROM_ENV_DEPTH: u8 = 25;

/// A [`boxed`][fn@ts::Layer::boxed] [`tracing subscriber`][mod@ts] [`Layer`][trait@ts::layer::Layer]
type BoxDynLayer<S> = Box<dyn ts::Layer<S> + Send + Sync>;
/// Represents the stack of layers this crate creates from a configuration file excluding the final `Vec` of dyn Layer.
type LayeredSubscriber =
    ts::layer::Layered<SpanRecordLayer, ts::layer::Layered<ts::EnvFilter, ts::Registry>>;
/// Represents a full [`tracing`][mod@t] [`Subscriber`][trait@t::Subscriber] implemented by [`Layered`][struct@ts::layer::Layered]
type TracingConfigSubscriber =
    ts::layer::Layered<Vec<BoxDynLayer<LayeredSubscriber>>, LayeredSubscriber>;

/// A [`TracingConfigGuard`] wrapped in an `Arc`<`Mutex`>
type ArcMutexGuard = Arc<Mutex<Guard>>;
/// A [`TracingConfigGuard`] wrapped in an `Weak`<`Mutex`>
type WeakMutexGuard = Weak<Mutex<Guard>>;

trait PushGuard<T> {
    fn push(&self, guard: T) -> Result<(), TracingConfigError>;
}

/// This guard contains data that must live for the entire lifetime of the program
/// on drop() it will flush all remaining tracing events to their destination before closing the program.
#[must_use = "Your program will randomly panic if you don't use the returned guard, do this : `let _tcg = tracing_config::init!{...};`"]
pub struct TracingConfigGuard {
    #[allow(dead_code)] // It's fine, we only use this for it's drop() and the "must_use"
    arc_mutex_guard: ArcMutexGuard,
}

impl TracingConfigGuard {
    fn new(arc_mutex_guard: ArcMutexGuard) -> Self {
        Self { arc_mutex_guard }
    }
}

/// A guard that flushes spans/events associated to asynchronous operations on a drop.
/// It should only be dropped in main, dropping this early will result in a panic depending on how tracing is configured.
#[must_use]
struct Guard {
    ta: Vec<ta::non_blocking::WorkerGuard>,
}
impl Guard {
    fn new() -> Self {
        Self { ta: Vec::new() }
    }
    fn new_arc_mutex() -> ArcMutexGuard {
        Arc::new(Mutex::new(Self::new()))
    }
}

impl PushGuard<ta::non_blocking::WorkerGuard> for WeakMutexGuard {
    fn push(&self, guard: ta::non_blocking::WorkerGuard) -> Result<(), TracingConfigError> {
        match self.upgrade() {
            Some(tc) => match tc.lock() {
                Ok(mut tc) => tc.ta.push(guard),
                Err(_e) => {
                    return Err(TracingConfigError::PoisonError(
                        "TracingConfigGuard".to_owned(),
                    ));
                }
            },
            None => return Err(TracingConfigError::TracingConfigGuardDropped),
        };
        Ok(())
    }
}

/// Helper macro to set layer.with_
macro_rules! set_conf {
    ($layer:ident, $cfg_layer:ident, $prop:ident, $with_fn:ident) => {
        match $cfg_layer.$prop {
            Some(val) => $layer.$with_fn(val),
            None => $layer,
        }
    };
}

// String representation for data model Level
impl AsRef<str> for model::Level {
    fn as_ref(&self) -> &str {
        match self {
            model::Level::Trace => "trace",
            model::Level::Debug => "debug",
            model::Level::Info => "info",
            model::Level::Warn => "warn",
            model::Level::Error => "error",
        }
    }
}

impl TryFrom<&str> for model::Level {
    type Error = TracingConfigError;

    fn try_from(value: &str) -> Result<Self, TracingConfigError> {
        match value.to_lowercase().as_str() {
            "trace" => Ok(model::Level::Trace),
            "debug" => Ok(model::Level::Debug),
            "info" => Ok(model::Level::Info),
            "warn" => Ok(model::Level::Warn),
            "error" => Ok(model::Level::Error),
            _ => Err(TracingConfigError::InvalidLevel {
                level: value.to_owned(),
            }),
        }
    }
}

/// Convert data model FileRotation to tracing appender
impl From<model::FileRotation> for ta::rolling::Rotation {
    fn from(value: model::FileRotation) -> Self {
        match value {
            model::FileRotation::Minutely => ta::rolling::Rotation::MINUTELY,
            model::FileRotation::Hourly => ta::rolling::Rotation::HOURLY,
            model::FileRotation::Daily => ta::rolling::Rotation::DAILY,
            model::FileRotation::Never => ta::rolling::Rotation::NEVER,
        }
    }
}

/// Convert data model SpanEvents to tracing subscriber
impl From<model::SpanEvents> for ts::fmt::format::FmtSpan {
    fn from(value: model::SpanEvents) -> Self {
        match value {
            model::SpanEvents::New => ts::fmt::format::FmtSpan::NEW,
            model::SpanEvents::Enter => ts::fmt::format::FmtSpan::ENTER,
            model::SpanEvents::Exit => ts::fmt::format::FmtSpan::EXIT,
            model::SpanEvents::Close => ts::fmt::format::FmtSpan::CLOSE,
            model::SpanEvents::None => ts::fmt::format::FmtSpan::NONE,
            model::SpanEvents::Active => ts::fmt::format::FmtSpan::ACTIVE,
            model::SpanEvents::Full => ts::fmt::format::FmtSpan::FULL,
        }
    }
}

/// Add functionality to data model FileWriter
impl model::FileWriter {
    /// Creates a tracing appender blocking [`RollingFileAppender`][struct@ta::rolling::RollingFileAppender].
    /// Whether enabled or not [`non_blocking : NonBlockingOptions`][struct@model::NonBlockingOptions] are ignored.
    fn create_rolling_file_appender(
        &self,
    ) -> Result<ta::rolling::RollingFileAppender, ta::rolling::InitError> {
        let mut builder =
            ta::rolling::RollingFileAppender::builder().filename_prefix(self.file_name.clone());

        if let Some(file_ext) = &self.file_ext {
            builder = builder.filename_suffix(file_ext);
        } else {
            builder = builder.filename_suffix("log".to_owned());
        }
        if let Some(max_log_files) = self.max_log_files {
            builder = builder.max_log_files(max_log_files)
        }
        if let Some(rotation) = &self.rotation {
            builder = builder.rotation((*rotation).into());
        } else {
            builder = builder.rotation(ta::rolling::Rotation::DAILY);
        }

        builder.build(self.directory_path.clone())
    }
    /// Creates a tracing appender [`NonBlocking`][struct@ta::non_blocking::NonBlocking] writer.
    /// If [`non_blocking : NonBlockingOptions`][struct@model::NonBlockingOptions] is disabled this still returns a non blocking writer with default non blocking properties.
    fn create_non_blocking(
        &self,
    ) -> Result<
        (ta::non_blocking::NonBlocking, ta::non_blocking::WorkerGuard),
        ta::rolling::InitError,
    > {
        let writer = self.create_rolling_file_appender()?;
        let mut nbb = ta::non_blocking::NonBlockingBuilder::default();

        if self.non_blocking.enabled {
            if let Some(buffered_lines_limit) = self.non_blocking.buffered_lines_limit {
                nbb = nbb.buffered_lines_limit(buffered_lines_limit);
            }
            if let Some(lossy) = self.non_blocking.lossy {
                nbb = nbb.lossy(lossy)
            } else {
                nbb = nbb.lossy(false)
            }
            if let Some(thread_name) = &self.non_blocking.thread_name {
                nbb = nbb.thread_name(thread_name.as_str());
            }
        }

        Ok(nbb.finish(writer))
    }
}

// Add functionality to data model SiftingLayer
impl model::SiftingLayer {
    // Creates a [`Selector`][struct@sifting_layer::Selector]
    fn get_sift_selector(&self) -> SiftingLayerSelector {
        let mut ss = SiftingLayerSelector::new();
        for sift_on in &self.sift_on {
            ss = match sift_on.as_str() {
                "${meta:level}" => ss.level(),
                "${meta:name}" => ss.name(),
                "${meta:target}" => ss.target(),
                "${meta:module_path}" => ss.module_path(),
                "${meta:file}" => ss.file(),
                "${meta:line}" => ss.line(),
                field => ss.field(field),
            }
        }

        ss
    }
}

/// Creates an [`EnvFilter`][struct@ts::filter::EnvFilter] given it's `cfg_filter_name` in the `tracing_config`
///
/// # Parameters
/// * `tracing_config` - A [`TracingConfig`][struct@model::TracingConfig] object containing the filter definition.
/// * `cfg_layer_name` - This is used solely for error reporting.
/// * `cfg_filter_name` - The name of the filter, if `None` returns Ok(None)
///
/// # Returns
///
/// If `cfg_filter_name` is `None` it returns `Ok(None)` otherwise returns `Some(EnvFilter)` or an [`FsTracingError`][enum@TracingConfigError] in case filter is missing or there are parse errors.
fn create_env_filter(
    tracing_config: &model::TracingConfig,
    cfg_layer_name: &str,
    cfg_filter_name: &Option<String>,
) -> Result<Option<ts::filter::EnvFilter>, TracingConfigError> {
    let cfg_filter_name = match cfg_filter_name {
        Some(cfg_filter_name) => cfg_filter_name,
        None => {
            return Ok(None);
        }
    };

    let cfg_filter = tracing_config.filters.get(cfg_filter_name).ok_or_else(|| {
        TracingConfigError::FilterNotFound {
            filter: cfg_filter_name.clone(),
            layer: cfg_layer_name.to_owned(),
        }
    })?;

    let mut env_filter = match ts::filter::EnvFilter::builder().parse(cfg_filter.level) {
        Ok(env_filter) => env_filter,
        Err(parse_error) => {
            return Err(TracingConfigError::FilterParseError {
                filter: cfg_filter_name.clone(),
                layer: cfg_layer_name.to_owned(),
                error: parse_error,
            });
        }
    };

    if let Some(directives) = &cfg_filter.directives {
        for directive in directives {
            let directive = match directive.parse() {
                Ok(directive) => directive,
                Err(parse_error) => {
                    return Err(TracingConfigError::FilterParseError {
                        filter: cfg_filter_name.clone(),
                        layer: cfg_layer_name.to_owned(),
                        error: parse_error,
                    });
                }
            };
            env_filter = env_filter.add_directive(directive);
        }
    }

    Ok(Some(env_filter))
}

/// Creates a [`boxed`][fn@ts::Layer::boxed] [`fmt::Layer`][struct@ts::fmt::Layer] given it's `cfg_layer_name` in the `tracing_config`.
///
/// # Parameters
/// * `tracing_config` - A [`TracingConfig`][struct@model::TracingConfig] object containing the layer and filter definitions.
/// * `cfg_layer_name` - The name of the layer as declared in `tracing_config`
/// * `cfg_layer` - The actual [`FmtLayer`][struct@model::FmtLayer] borrowed from the `tracing_config` object.
/// * `cfg_writer` - A writer override, if `None` the writer will be retrieved from the `tracing_config` object given `cfg_layer.writer` and will error out with an [`TracingConfigError::WriterNotFound`][type@TracingConfigError::WriterNotFound] if not found.
/// * `guard` - If the writer is async the tracing appender worker guard will be pushed here
///
/// # Returns
/// A [`boxed`][fn@ts::Layer::boxed] [`fmt::Layer`][struct@ts::fmt::Layer] configured given `cfg_layer` or an [`TracingConfigError`][enum@TracingConfigError].
fn create_fmt_layer<S>(
    tracing_config: &model::TracingConfig,
    cfg_layer_name: &str,
    cfg_layer: &model::FmtLayer,
    cfg_writer: Option<&model::Writer>,
    guard: WeakMutexGuard,
) -> Result<BoxDynLayer<S>, TracingConfigError>
where
    S: t::Subscriber,
    S: for<'lookup> ts::registry::LookupSpan<'lookup>,
    S: Send + Sync,
{
    use ts::fmt::format::Compact as FormatCompact;
    use ts::fmt::format::Format as TsFormat;
    use ts::fmt::format::Full as FormatFull;
    use ts::fmt::format::Json as FormatJson;
    use ts::fmt::format::JsonFields as FormatJsonFields;
    use ts::fmt::format::Pretty as FormatPretty;
    use ts::fmt::Layer as FmtLayer;
    enum LayerFormatted<S, F, W, T> {
        // S = subscriber lookup span,
        // F = format fields
        // W = writer,
        // T = format fields time
        Full(FmtLayer<S, F, TsFormat<FormatFull, T>, W>),
        Compact(FmtLayer<S, F, TsFormat<FormatCompact, T>, W>),
        Pretty(FmtLayer<S, FormatPretty, TsFormat<FormatPretty, T>, W>),
        Json(FmtLayer<S, FormatJsonFields, TsFormat<FormatJson, T>, W>),
    }

    macro_rules! box_layer {
        ($fmt_layer:expr, $writer:expr, $filter:expr) => {
            match $filter {
                Some(filter) => match $fmt_layer {
                    LayerFormatted::Full(fmt_layer) => {
                        fmt_layer.with_writer($writer).with_filter(filter).boxed()
                    }
                    LayerFormatted::Compact(fmt_layer) => {
                        fmt_layer.with_writer($writer).with_filter(filter).boxed()
                    }
                    LayerFormatted::Pretty(fmt_layer) => {
                        fmt_layer.with_writer($writer).with_filter(filter).boxed()
                    }
                    LayerFormatted::Json(fmt_layer) => {
                        fmt_layer.with_writer($writer).with_filter(filter).boxed()
                    }
                },
                None => match $fmt_layer {
                    LayerFormatted::Full(fmt_layer) => fmt_layer.with_writer($writer).boxed(),
                    LayerFormatted::Compact(fmt_layer) => fmt_layer.with_writer($writer).boxed(),
                    LayerFormatted::Pretty(fmt_layer) => fmt_layer.with_writer($writer).boxed(),
                    LayerFormatted::Json(fmt_layer) => fmt_layer.with_writer($writer).boxed(),
                },
            }
        };
    }

    let cfg_writer = match cfg_writer {
        Some(cfg_writer) => cfg_writer,
        None => tracing_config
            .writers
            .get(&cfg_layer.writer)
            .ok_or_else(|| TracingConfigError::WriterNotFound {
                writer: cfg_layer.writer.clone(),
                layer: cfg_layer_name.to_owned(),
            })?,
    };

    let env_filter = create_env_filter(tracing_config, cfg_layer_name, &cfg_layer.filter)?;

    let mut fmt_layer = ts::fmt::Layer::new().with_ansi(cfg_layer.ansi);

    // TODO !
    // pub time : Option<bool>,

    fmt_layer = set_conf!(fmt_layer, cfg_layer, level, with_level);
    fmt_layer = set_conf!(fmt_layer, cfg_layer, target, with_target);
    fmt_layer = set_conf!(fmt_layer, cfg_layer, file, with_file);
    fmt_layer = set_conf!(fmt_layer, cfg_layer, line_number, with_line_number);
    fmt_layer = set_conf!(fmt_layer, cfg_layer, thread_ids, with_thread_ids);
    fmt_layer = set_conf!(fmt_layer, cfg_layer, thread_names, with_thread_names);

    fmt_layer = fmt_layer.with_span_events(cfg_layer.span_events.into());

    let fmt_layer = match cfg_layer.formatter {
        model::FmtLayerFormatter::Full => LayerFormatted::Full(fmt_layer),
        model::FmtLayerFormatter::Compact => LayerFormatted::Compact(fmt_layer.compact()),
        model::FmtLayerFormatter::Pretty => LayerFormatted::Pretty(fmt_layer.pretty()),
        model::FmtLayerFormatter::Json => {
            let mut fmt_json_layer = fmt_layer.json();

            fmt_json_layer = set_conf!(fmt_json_layer, cfg_layer, span_list, with_span_list);
            fmt_json_layer = set_conf!(fmt_json_layer, cfg_layer, current_span, with_current_span);

            fmt_json_layer = match cfg_layer.flatten_event {
                Some(flatten_event) => fmt_json_layer.flatten_event(flatten_event),
                None => fmt_json_layer,
            };
            LayerFormatted::Json(fmt_json_layer)
        }
    };

    // writer is the second-last to be set, finally the filter

    Ok(match cfg_writer {
        model::Writer::File(cfg_file_writer) => {
            if cfg_file_writer.non_blocking.enabled {
                let (writer, writer_guard) = cfg_file_writer.create_non_blocking()?;
                guard.push(writer_guard)?;
                box_layer!(fmt_layer, writer, env_filter)
            } else {
                let writer = cfg_file_writer.create_rolling_file_appender()?;
                box_layer!(fmt_layer, writer, env_filter)
            }
        }
        model::Writer::StandardOutput => {
            let writer = std::io::stdout;
            box_layer!(fmt_layer, writer, env_filter)
        }
    })
}

/// Creates a [`boxed`][fn@ts::Layer::boxed] [`JsonLayer`][struct@JsonLayer] given it's `cfg_layer_name` in the `tracing_config`.
///
/// # Parameters
/// * `tracing_config` - A [`TracingConfig`][struct@model::TracingConfig] object containing the layer and filter definitions.
/// * `cfg_layer_name` - The name of the layer as declared in `tracing_config`
/// * `cfg_layer` - The actual [`JsonLayer`][struct@JsonLayer] borrowed from the `tracing_config` object.
/// * `cfg_writer` - A writer override, if `None` the writer will be retrieved from the `tracing_config` object given `cfg_layer.writer` and will error out with an [`TracingConfigError::WriterNotFound`][type@TracingConfigError::WriterNotFound] if not found.
/// * `ta_worker_guards` - If the writer is async the tracing appender worker guard will be pushed here
///
/// # Returns
/// A [`boxed`][fn@ts::Layer::boxed] [`JsonLayer`][struct@JsonLayer] configured given `cfg_layer` or an [`TracingError`][enum@TracingConfigError].
fn create_json_layer<S>(
    tracing_config: &model::TracingConfig,
    cfg_layer_name: &str,
    cfg_layer: &model::JsonLayer,
    cfg_writer: Option<&model::Writer>,
    guard: WeakMutexGuard,
) -> Result<BoxDynLayer<S>, TracingConfigError>
where
    S: t::Subscriber,
    S: for<'lookup> ts::registry::LookupSpan<'lookup>,
    S: Send + Sync,
{
    macro_rules! json_layer_set_conf {
        ($layer:ident, $cfg_layer:ident) => {
            $layer = set_conf!($layer, cfg_layer, trailing_comma, with_trailing_comma);
            $layer = set_conf!($layer, cfg_layer, pretty_json, with_pretty_json);
            $layer = set_conf!($layer, cfg_layer, span_id, with_span_id);
            $layer = set_conf!($layer, cfg_layer, span_uuid, with_span_uuid);
            $layer = set_conf!($layer, cfg_layer, span_timestamp, with_span_timestamp);
            $layer = set_conf!($layer, cfg_layer, span_level, with_span_level);
            $layer = set_conf!($layer, cfg_layer, span_name, with_span_name);
            $layer = set_conf!($layer, cfg_layer, span_target, with_span_target);
            $layer = set_conf!($layer, cfg_layer, span_module_path, with_span_module_path);
            $layer = set_conf!($layer, cfg_layer, span_file, with_span_file);
            $layer = set_conf!($layer, cfg_layer, span_line, with_span_line);
            $layer = set_conf!($layer, cfg_layer, span_fields, with_span_fields);
            $layer = set_conf!($layer, cfg_layer, event_timestamp, with_event_timestamp);
            $layer = set_conf!($layer, cfg_layer, event_level, with_event_level);
            $layer = set_conf!($layer, cfg_layer, event_name, with_event_name);
            $layer = set_conf!($layer, cfg_layer, event_target, with_event_target);
            $layer = set_conf!($layer, cfg_layer, event_module_path, with_event_module_path);
            $layer = set_conf!($layer, cfg_layer, event_file, with_event_file);
            $layer = set_conf!($layer, cfg_layer, event_line, with_event_line);
            $layer = set_conf!($layer, cfg_layer, event_fields, with_event_fields);
            $layer = set_conf!($layer, cfg_layer, event_span_id, with_event_span_id);
            $layer = set_conf!($layer, cfg_layer, event_span_uuid, with_event_span_uuid);
            $layer = set_conf!($layer, cfg_layer, event_spans, with_event_spans);
        };
    }

    let cfg_writer = match cfg_writer {
        Some(cfg_writer) => cfg_writer,
        None => tracing_config
            .writers
            .get(&cfg_layer.writer)
            .ok_or_else(|| TracingConfigError::WriterNotFound {
                writer: cfg_layer.writer.clone(),
                layer: cfg_layer_name.to_owned(),
            })?,
    };

    let env_filter = create_env_filter(tracing_config, cfg_layer_name, &cfg_layer.filter)?;

    Ok(match cfg_writer {
        model::Writer::File(cfg_file_writer) => {
            if cfg_file_writer.non_blocking.enabled {
                let (writer, writer_guard) = cfg_file_writer.create_non_blocking()?;
                guard.push(writer_guard)?;
                let mut json_layer = JsonLayer::new(writer);
                json_layer_set_conf!(json_layer, cfg_layer);
                match env_filter {
                    Some(env_filter) => json_layer.with_filter(env_filter).boxed(),
                    None => json_layer.boxed(),
                }
            } else {
                let writer = cfg_file_writer.create_rolling_file_appender()?;
                let mut json_layer = JsonLayer::new(writer);
                json_layer_set_conf!(json_layer, cfg_layer);
                match env_filter {
                    Some(env_filter) => json_layer.with_filter(env_filter).boxed(),
                    None => json_layer.boxed(),
                }
            }
        }
        model::Writer::StandardOutput => {
            let writer = std::io::stdout;
            let mut json_layer = JsonLayer::new(writer);
            json_layer_set_conf!(json_layer, cfg_layer);
            match env_filter {
                Some(env_filter) => json_layer.with_filter(env_filter).boxed(),
                None => json_layer.boxed(),
            }
        }
    })
}

/// Creates a [`boxed`][fn@ts::Layer::boxed] [`SiftingLayer`][struct@SiftingLayer] given it's `cfg_layer_name` in the `tracing_config`.
///
/// The implementation is crude but it works.
///
/// # Parameters
/// * `tracing_config` - A [`TracingConfig`][struct@model::TracingConfig] object containing the layer and filter definitions as well as the sifted layer and sifted writer definitions.
/// * `cfg_layer_name` - The name of the layer as declared in `tracing_config`
/// * `cfg_layer` - The actual [`SiftingLayer`][struct@model::SiftingLayer] borrowed from the `tracing_config` object.
/// * `ta_worker_guards` - If the writer is async the tracing appender worker guard will be pushed here
///
/// # Returns
/// A [`boxed`][fn@ts::Layer::boxed] [`SiftingLayer`][struct@SiftingLayer] configured given `cfg_layer` or an [`TracingConfigError`][enum@TracingConfigError] (e.g.: if the sifted layer or writer are not present).
fn create_sifting_layer<S>(
    verbosity: Option<model::Level>,
    tracing_config: &model::TracingConfig,
    cfg_layer_name: &str,
    cfg_layer: &model::SiftingLayer,
    guard: WeakMutexGuard,
) -> Result<BoxDynLayer<S>, TracingConfigError>
where
    S: t::Subscriber,
    S: for<'lookup> ts::registry::LookupSpan<'lookup>,
    S: Send + Sync,
{
    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    enum SiftedLayer {
        Fmt(model::FmtLayer),
        Json(model::JsonLayer),
    }

    let tracing_config = tracing_config.clone();
    let cfg_layer_name = cfg_layer_name.to_owned();

    let cfg_sifted_writer = tracing_config
        .writers
        .get(&cfg_layer.writer)
        .ok_or_else(|| TracingConfigError::WriterNotFound {
            writer: cfg_layer.writer.clone(),
            layer: cfg_layer_name.to_owned(),
        })?
        .clone();

    let cfg_sifted_layer = tracing_config
        .layers
        .get(&cfg_layer.layer)
        .ok_or_else(|| TracingConfigError::LayerNotFound {
            sifted_layer: cfg_layer.layer.clone(),
            layer: cfg_layer_name.to_owned(),
        })?
        .clone();

    let cfg_sifted_layer = match cfg_sifted_layer {
        model::Layer::Fmt(cfg_sifted_layer) => SiftedLayer::Fmt(cfg_sifted_layer),
        model::Layer::Json(cfg_sifted_layer) => SiftedLayer::Json(cfg_sifted_layer),
        model::Layer::Sifting(_) => {
            return Err(TracingConfigError::SiftingLayerConf {
                sifted_layer: cfg_layer.layer.clone(),
                layer: cfg_layer_name.to_owned(),
                error_message: "the sifted layer cannot be itself a sifting layer".to_owned(),
            });
        }
    };

    let env_filter = create_env_filter(&tracing_config, &cfg_layer_name, &cfg_layer.filter)?;

    let selector = cfg_layer.get_sift_selector();

    let sift_layer = SiftingLayer::new(selector, move |ss, ssv| {
        let cfg_writer = match cfg_sifted_writer.clone() {
            model::Writer::File(mut cfg_writer) => {
                cfg_writer.directory_path = ss.resolve_variable(&cfg_writer.directory_path, ssv);
                cfg_writer.file_name = ss.resolve_variable(&cfg_writer.file_name, ssv);
                cfg_writer.file_ext = cfg_writer
                    .file_ext
                    .map(|file_ext| ss.resolve_variable(&file_ext, ssv));
                // TODO! somehow include this in the config file, or document it
                const SANITIZE_OPTIONS: sanitise_file_name::Options<Option<char>> =
                    sanitise_file_name::Options {
                        most_fs_safe: true,
                        windows_safe: true,
                        replace_with: Some('-'),
                        collapse_replacements: true,
                        six_measures_of_barley: "none",
                        ..sanitise_file_name::Options::DEFAULT
                    };
                emit!(INFO, verbosity, "cfg_writer = {cfg_writer:?}");
                // cfg_writer.directory_path = sanitise_file_name::sanitize_with_options(&cfg_writer.directory_path, &SANITIZE_OPTIONS);
                cfg_writer.file_name = sanitise_file_name::sanitize_with_options(
                    &cfg_writer.file_name,
                    &SANITIZE_OPTIONS,
                );
                cfg_writer.file_ext = cfg_writer.file_ext.map(|file_ext| {
                    sanitise_file_name::sanitize_with_options(&file_ext, &SANITIZE_OPTIONS)
                });
                emit!(INFO, verbosity, "cfg_writer = {cfg_writer:?}");
                model::Writer::File(cfg_writer)
            }
            model::Writer::StandardOutput => model::Writer::StandardOutput,
        };

        match cfg_sifted_layer.clone() {
            SiftedLayer::Fmt(cfg_sifted_layer) => {
                let layer = match create_fmt_layer(
                    &tracing_config,
                    &cfg_layer_name,
                    &cfg_sifted_layer,
                    Some(&cfg_writer),
                    guard.clone(),
                ) {
                    Ok(layer) => layer,
                    Err(err) => {
                        emit!(
                            ERROR,
                            verbosity,
                            "Could not create sifted fmt layer `{}`, please review the configuration or open bug report, the error is: {}",
                            cfg_layer_name,
                            err
                        );
                        // TODO: this needs work,
                        // a panic here is most likely a configuration file mistake
                        // or maybe we should modify the sifting layer builder and allow it to return an error ?
                        // but then the sifting layer would have to cope with it ...
                        // maybe it should fallback to standard output with a basic tracing subscriber fmt layer ...
                        panic!(
                            "Could not create sifted fmt layer `{}`, please review the configuration or open bug report, the error is: {}",
                            cfg_layer_name, err
                        );
                    }
                };
                layer.boxed()
            }
            SiftedLayer::Json(cfg_sifted_layer) => {
                let layer = match create_json_layer(
                    &tracing_config,
                    &cfg_layer_name,
                    &cfg_sifted_layer,
                    Some(&cfg_writer),
                    guard.clone(),
                ) {
                    Ok(layer) => layer,
                    Err(err) => {
                        emit!(
                            ERROR,
                            verbosity,
                            "Could not create sifted fmt layer `{}`, please review the configuration or open bug report, the error is: {}",
                            cfg_layer_name,
                            err
                        );
                        // TODO: this needs work,
                        // a panic here is most likely a configuration file mistake
                        panic!(
                            "Could not create sifted fmt layer `{}`, please review the configuration or open bug report, the error is: {}",
                            cfg_layer_name, err
                        );
                    }
                };
                layer.boxed()
            }
        }
    });

    Ok(match env_filter {
        Some(env_filter) => sift_layer.with_filter(env_filter).boxed(),
        None => sift_layer.boxed(),
    })
}

/// Creates a [`Vec`][struct@Vec] of [`boxed`][fn@ts::Layer::boxed] [`Layer`][trait@ts::layer::Layer] given all the `layers` in `tracing_config` in no particular order.
///
/// [`tracing subscriber`][mod@ts] implements [`Layer`][trait@ts::layer::Layer] for [`Vec`][struct@Vec] so the end result is essentially a `Layer`
///
/// # Parameters
/// * `tracing_config` - A [`TracingConfig`][struct@model::TracingConfig] object containing the layer and filter and writer definitions.
///
/// # Returns
/// A tuple of [`Vec`][struct@Vec] of [`boxed`][fn@ts::Layer::boxed] [`Layer`][trait@ts::layer::Layer] and a [`ArcMutexGuard`][type@ArcMutexGuard] or an [`TracingConfigError`][enum@TracingConfigError].
fn create_layers<S>(
    verbosity: Option<model::Level>,
    tracing_config: &model::TracingConfig,
) -> Result<(Vec<BoxDynLayer<S>>, ArcMutexGuard), TracingConfigError>
where
    S: t::Subscriber,
    S: for<'lookup> ts::registry::LookupSpan<'lookup>,
    S: Send + Sync,
{
    let guard = Guard::new_arc_mutex();

    let mut layers: Vec<BoxDynLayer<S>> = Vec::new();

    for (cfg_layer_name, cfg_layer) in &tracing_config.layers {
        // skip layers having writer = "${sl:sifted}" because they are sifted layers
        // return error if they have a filter applied
        match cfg_layer {
            model::Layer::Fmt(cfg_layer) => {
                if cfg_layer.writer == "${sl:sifted}" {
                    if let Some(filter) = &cfg_layer.filter {
                        return Err(TracingConfigError::SiftingLayerConf {
                            sifted_layer: cfg_layer_name.to_owned(),
                            layer: "not parsed yet".to_owned(),
                            error_message: format!(
                                "sifted layers may not have filters, found filter `{filter}`"
                            ),
                        });
                    }
                    continue;
                }
            }
            model::Layer::Json(cfg_layer) => {
                if cfg_layer.writer == "${sl:sifted}" {
                    if let Some(filter) = &cfg_layer.filter {
                        return Err(TracingConfigError::SiftingLayerConf {
                            sifted_layer: cfg_layer_name.to_owned(),
                            layer: "not parsed yet".to_owned(),
                            error_message: format!(
                                "sifted layers may not have filters, found filter `{filter}`"
                            ),
                        });
                    }
                    continue;
                }
            }
            model::Layer::Sifting(_) => (),
        }

        match cfg_layer {
            model::Layer::Fmt(cfg_layer) => {
                layers.push(create_fmt_layer(
                    tracing_config,
                    cfg_layer_name,
                    cfg_layer,
                    None,
                    Arc::downgrade(&guard),
                )?);
            }
            model::Layer::Json(cfg_layer) => {
                layers.push(create_json_layer(
                    tracing_config,
                    cfg_layer_name,
                    cfg_layer,
                    None,
                    Arc::downgrade(&guard),
                )?);
            }
            model::Layer::Sifting(cfg_layer) => {
                layers.push(create_sifting_layer(
                    verbosity,
                    tracing_config,
                    cfg_layer_name,
                    cfg_layer,
                    Arc::downgrade(&guard),
                )?);
            }
        }
    }

    Ok((layers, guard))
}

/// Creates an [`EnvFilter`][struct@ts::filter::EnvFilter] given the filter name `root` in the `tracing_config`
///
/// # Parameters
/// * `tracing_config` - A [`TracingConfig`][struct@model::TracingConfig] object containing the filter definition.
///
/// # Returns
/// The root `EnvFilter` or an [`TracingError`][enum@TracingConfigError] in case filter is missing or there are parse errors.
fn create_root_filter(
    tracing_config: &model::TracingConfig,
) -> Result<ts::filter::EnvFilter, TracingConfigError> {
    let root_filter = create_env_filter(tracing_config, "root_registry", &Some("root".to_owned()))?;

    let root_filter = match root_filter {
        Some(f) => f,
        None => {
            return Err(TracingConfigError::FilterNotFound {
                filter: "root".to_owned(),
                layer: "root_registry".to_owned(),
            });
        }
    };

    Ok(root_filter)
}

/// Creates a [`tracing`][mod@t] [`Subscriber`][trait@t::Subscriber] implemented by a [`tracing-subscriber`][mod@ts] [`Layered`][struct@ts::layer::Layered] [`Registry`][struct@ts::registry::Registry]
///
/// The layered registry is comprised of the following; starting from the root layer (the first one that is called by the actual tracing subscriber, which in turn will call the next and so on...)
/// - The root [`EnvFilter`][struct@ts::EnvFilter] configured by the special filter named `root`.
/// - The [`SpanRecordLayer`][struct@SpanRecordLayer] which will save all (non globally filtered out) span data values in the span's [`Extensions`][struct@ts::registry::Extensions]
/// - The Vec of all [`boxed`][fn@ts::Layer::boxed] layers declared in the configuration file in no particular order.
///
/// # Parameters
/// * `tracing_config` - A [`TracingConfig`][struct@model::TracingConfig] object containing everything.
///
/// # Returns
/// A `Result`
/// - The `Ok` variant being a tuple of ([`TracingConfigSubscriber`][type@TracingConfigSubscriber], [`ArcMutexGuard`][type@ArcMutexGuard])
/// - The `Err` variant being an [`TracingConfigError`][enum@TracingConfigError] in case a subscriber could not be build from the `tracing_config` parameter.
///
/// - The [`TracingConfigSubscriber`][type@TracingConfigSubscriber] is the [`Subscriber`][trait@t::Subscriber] that should be passed to to the [`set_global_default`][fn@t::subscriber::set_global_default] function in tracing.
/// - The [`ArcMutexGuard`][type@ArcMutexGuard] contains [`tracing appender`][mod@ta] [`WorkerGuard`][struct@ta::non_blocking::WorkerGuard]s and should only be dropped at the end of the program.
///
/// # Understanding the [TracingConfigSubscriber][type@TracingConfigSubscriber] type
/// A [`Layer`][ts::layer::Layer]<`S` : `Subscriber`> by itself is not a [`tracing`][mod@t] [`Subscriber`][trait@t::Subscriber], instead an actual `Subscriber` will forward it events coming from tracing.
/// However a generic type parameter `S` is required for the layer to receive [`Context`][struct@ts::layer::Context] from the actual subscriber, thus we say that a `Layer` "wraps" a `Subscriber`.
///
/// A [`Layered`][ts::layer::Layered]<`L`, `I`, `S` = `I`> is such Subscriber; in the diagram `L` is `layer` (a Layer), `I` is `inner` (a Layer) and `S` is a `Subscriber`; `I`=`S` means that `inner` must be both a `Subscriber` and a `Layer`.
/// A [`Layered`][ts::layer::Layered] will forward messages/events from tracing first to `inner` (the `Subscriber`) and then to `layer`,
/// this is because [`Layered`][struct@ts::layer::Layered] is not an actual `Subscriber` since `inner` is the actual `Subscriber` and there can only be 1 real `Subscriber` as suggested by the tracing [`set_global_default`][fn@t::subscriber::set_global_default] api.
///
/// Both [`EnvFilter`][struct@ts::EnvFilter] and [`SpanRecordLayer`][struct@SpanRecordLayer] are only layers, they do not implements `Subscriber`.
///
/// This leaves us with [`Registry`][struct@ts::registry::Registry] which is the actual `Subscriber`.
///
/// Following the diagram below we can see that [`TracingConfigSubscriber`][type@TracingConfigSubscriber] is a `Layered` where:
/// - The `Layer` is a `Vec` (yes `Vec` implements `Layer`) `Vec<Box<dyn ts::Layer<S> + Send + Sync>>` where `S` is the "wrapped" subscriber, that is: it should have the same type as the actual subscriber that embeds it or calls it. In this case the subscriber that embeds it is the `inner` of the top level `Layered`.
/// - The `inner` is itself another `Layered` adding the `SpanRecordLayer`
/// - Next is yet another `Layered` having `layer = ts::EnvFilter` and `inner = ts::Registry`
///
/// Given that `Layered` is a `Subscriber` we can follow the chain:
/// - L1 first calls `inner` which is L2
/// - L2 first calls `inner` which is L3
/// - L3 first calls `inner` which is `ts::Registry` this is the actual Subscriber
/// - L3 calls `layer` which is the `ts::EnvFilter` and returns to L2
/// - L2 calls `layer` which is the `SpanRecordLayer` and returns to L1
/// - L1 calls `layer` which is the `Vec` (at this level the `Subscriber` is L2) which is why the generic type parameter `S` for `Vec<Box<dyn ts::Layer<S> + Send + Sync>>` must be the same type as L2
///
/// - L1 is [`TracingConfigSubscriber`][type@TracingConfigSubscriber]
/// - L2 is [`LayeredSubscriber`][type@LayeredSubscriber]
///
/// This is what it expands to :
/// ```doc
/// // ts::layer::Layered< *L1
/// //     layer = Vec<Box<dyn ts::Layer<
/// //     |           subscriber = ts::layer::Layered<
/// //     |           |                layer = SpanRecordLayer,
/// //     |           |                inner = ts::layer::Layered<
/// //     |           |                |           layer = ts::EnvFilter,
/// //     |           |                |           inner = ts::Registry
/// //     |           |                +------ >
/// //     |           +----------- >
/// //     +------ > + Send + Sync>>,
/// //     inner = ts::layer::Layered< *L2
/// //     |           layer = SpanRecordLayer,
/// //     |           inner = ts::layer::Layered< *L3
/// //     |           |           layer = ts::EnvFilter,
/// //     |           |           inner = ts::Registry
/// //     |           +------ >
/// //     +------ >
/// // >
/// ```
fn create_subscriber(
    verbosity: Option<model::Level>,
    tracing_config: &model::TracingConfig,
) -> Result<(TracingConfigSubscriber, ArcMutexGuard), TracingConfigError> {
    let (layers, guard) = create_layers(verbosity, tracing_config)?;

    // the final tracing subscriber calls layers from top to bottom as they are defined in source.
    // registry -> filter -> span values layer -> layers
    let registry = ts::registry::Registry::default()
        .with(create_root_filter(tracing_config)?)
        .with(SpanRecordLayer::new())
        .with(layers);

    Ok((registry, guard))
}

/// Writes `config` to `file_path`.
///
/// This is useful if you don't want to bother to read the docs, instead you let the compiler guide you on what fields the various data structures have.
///
/// If you wish to manually write the config file, read the docs, start from here : [`TracingConfig`][struct@model::TracingConfig]
///
/// # Parameters
///
/// * `config` - A [`TracingConfig`][struct@model::TracingConfig] object to be written to desk as a toml file.
/// * `file_path` - The destination file [`Path`][struct@Path]
///
/// # Returns
///
/// An [`TracingConfigError`][enum@TracingConfigError] in case the operation could not be completed.
///
/// # Usage
/// ```
/// # use tracing_config::interpolate::resolve_from_env;
/// # fn f() -> Result<(), Box<dyn std::error::Error>> {
/// let file_path = Path::new("/home/user/temp/tracing.toml");
/// let mut filters = HashMap::new();
/// let mut writers = HashMap::new();
/// let mut layers = HashMap::new();
/// // Create a Config object
/// let config = TracingConfig {
///     title: "Test Config".into(),
///     filters,
///     writers,
///     layers,
/// };
/// write_config(&config, file_path)?;
/// # Ok(()) }
/// ```
pub fn write_config(
    config: &model::TracingConfig,
    file_path: &Path,
) -> Result<(), TracingConfigError> {
    // Serialize the config to a TOML string
    let toml_string = toml::to_string(config)?;

    // Write the TOML string to a file
    let mut file = fs::File::create(file_path)?;
    file.write_all(toml_string.as_bytes())?;

    Ok(())
}

/// Reads a [`TracingConfig`][struct@model::TracingConfig] from `file_path`.
///
/// This is useful if you wish to [`initialize`][fn@initialize] yourself.
///
/// This function also resolves all environment variables `${env:key}` up to `resolve_from_env_depth` for all [`toml String`][type@toml::Value::String] (see [`resolve_from_env_recursive`][fn@crate::interpolate::toml::resolve_from_env_recursive])
///
/// If you wish to manually read the config file, use the [`toml`][mod@toml] crate.
///
/// # Parameters
///
/// * `file_path` - The tracing.toml source file [`Path`][struct@Path]
/// * `resolve_from_env_depth` - Replaces all `${env:key}` placeholders in all [`toml String`][type@toml::Value::String] values with the value of the environment variable `key` recursively up to `resolve_from_env_depth` times (see [`resolve_from_env_recursive`][fn@crate::interpolate::toml::resolve_from_env_recursive]).
///
/// # Returns
///
/// A [`TracingConfig`][struct@model::TracingConfig] object or an [`TracingConfigError`][enum@TracingConfigError] in case the operation could not be completed.
///
/// # Usage
/// ```
/// # use tracing_config::interpolate::resolve_from_env;
/// # fn f() -> Result<(), Box<dyn std::error::Error>> {
/// let file_path = Path::new("/home/user/temp/tracing.toml");
/// let deserialized_config = read_config(file_path, RESOLVE_FROM_ENV_DEPTH)?;
/// println!("Deserialized Config: {:#?}", deserialized_config);
/// # Ok(()) }
/// ```
pub fn read_config(
    file_path: &Path,
    resolve_from_env_depth: u8,
) -> Result<model::TracingConfig, TracingConfigError> {
    // Read the TOML string from the file
    let toml_string = fs::read_to_string(file_path)?;

    // Deserialize the TOML string to a Config instance
    let mut toml_value: toml::Value = toml::from_str(&toml_string)?;
    crate::interpolate::toml::resolve_from_env_recursive(&mut toml_value, resolve_from_env_depth)?;

    let deserialized_config: model::TracingConfig = toml_value.try_into()?;

    Ok(deserialized_config)
}

/// Searches for a configuration file and returns it's path.
///
///
/// # Arguments
///
/// * `qualifier` - See [`ProjectDirs`]
/// * `organization` - See [`ProjectDirs`]
/// * `name` - See [`ProjectDirs`]
/// * `test` - If `true` the `search path` will prioritize `.toml` files that have a `-test` suffix.
/// * `verbosity` - How much to print to stdout
/// * `env` - Key of additional environment variable
/// * `path` - Direct path to configuration file or dir.
///
/// # Returns
///
/// * `Option<PathBuf>` - The path to the configuration file if found, otherwise `None`.
///
#[doc = include_str!("../doc/configuration_file_search_path.md")]
///
#[doc = include_str!("../doc/reference_links.md")]
pub fn find_config_path(
    qualifier: &str,
    organization: &str,
    name: &str,
    test: bool,
    verbosity: Option<model::Level>,
    env: Option<&str>,
    path: Option<&Path>,
) -> Option<PathBuf> {
    use std::env;

    fn env_var_with_resolve(key: &str, verbosity: Option<model::Level>) -> Option<String> {
        match env::var(key) {
            Ok(env_val) => {
                match resolve_from_env_recursive(env_val.as_str(), RESOLVE_FROM_ENV_DEPTH, "env") {
                    Ok(ok) => Some(ok),
                    Err(var_error) => {
                        match var_error {
                            crate::interpolate::VarError::NotPresent { key: err_key } => {
                                emit!(
                                    WARN,
                                    verbosity,
                                    "Could not resolve ${{env:{err_key}}} token, in resolve chain for '{key}'; '{err_key}' is not present."
                                );
                            }
                            crate::interpolate::VarError::NotUnicode {
                                key: err_key,
                                value: err_value,
                            } => {
                                emit!(
                                    WARN,
                                    verbosity,
                                    "Could not resolve ${{env:{err_key}}} token, in resolve chain for '{key}'; '{err_key}' \
                                    is present but is not unicode : {:?}.",
                                    err_value
                                );
                            }
                        }
                        Some(env_val)
                    }
                }
            }
            Err(var_error) => {
                match var_error {
                    env::VarError::NotPresent => {
                        emit!(
                            TRACE,
                            verbosity,
                            "Environment variable '{key}' is not present."
                        )
                    }
                    env::VarError::NotUnicode(os_string) => {
                        emit!(
                            WARN,
                            verbosity,
                            "Environment variable '{key}' is present but is not unicode : {:?}.",
                            os_string
                        )
                    }
                }
                None
            }
        }
    }

    let config_filenames = if test {
        vec![
            format!("tracing-{}-test.toml", name),
            "tracing-test.toml".to_string(),
            format!("tracing-{}.toml", name),
            "tracing.toml".to_string(),
        ]
    } else {
        vec![format!("tracing-{}.toml", name), "tracing.toml".to_string()]
    };

    emit!(
        DEBUG,
        verbosity,
        "Searching config paths for {} configuration",
        (if test { "test" } else { "prod" })
    );

    let path = path.map(|path| PathBuf::from(path));
    let env = match env {
        Some(env) => env_var_with_resolve(env, verbosity).map(|string| PathBuf::from(string)),
        None => None,
    };
    let tracing_config =
        env_var_with_resolve(ENV_TRACING_CONFIG, verbosity).map(|string| PathBuf::from(string));
    let tracing_config_test = if test {
        env_var_with_resolve(ENV_TRACING_CONFIG_TEST, verbosity).map(|string| PathBuf::from(string))
    } else {
        None
    };

    let (project_dirs_preference_dir, project_dirs_config_dir, project_dirs_config_local_dir) =
        match directories::ProjectDirs::from(qualifier, organization, name) {
            Some(project_dirs) => (
                Some(project_dirs.preference_dir().to_path_buf()),
                Some(project_dirs.config_dir().to_path_buf()),
                Some(project_dirs.config_local_dir().to_path_buf()),
            ),
            None => (None, None, None),
        };

    let (
        base_dirs_preference_dir,
        base_dirs_config_dir,
        base_dirs_config_local_dir,
        base_dirs_home_dir,
    ) = match directories::BaseDirs::new() {
        Some(base_dirs) => (
            Some(base_dirs.preference_dir().to_path_buf()),
            Some(base_dirs.config_dir().to_path_buf()),
            Some(base_dirs.config_local_dir().to_path_buf()),
            Some(base_dirs.home_dir().to_path_buf()),
        ),
        None => (None, None, None, None),
    };

    let user_dirs_home_dir = match directories::UserDirs::new() {
        Some(user_dirs) => Some(user_dirs.home_dir().to_path_buf()),
        None => None,
    };

    let current_exe_dir = env::current_exe()
        .ok()
        .and_then(|dir| dir.parent().map(|parent| parent.to_path_buf()));
    let current_dir = env::current_dir().ok();

    let mut search_path = Vec::new();

    search_path.push(("path", path));

    if let Some(env) = &env {
        if !env.exists() {
            search_path.push((
                "env_parent",
                env.parent().map(|parent| parent.to_path_buf()),
            ));
        }
    }

    search_path.push(("env", env));

    if test {
        if let Some(tracing_config_test) = &tracing_config_test {
            if !tracing_config_test.exists() {
                search_path.push((
                    ENV_TRACING_CONFIG_TEST_PARENT,
                    tracing_config_test
                        .parent()
                        .map(|parent| parent.to_path_buf()),
                ));
            }
        }
        search_path.push((ENV_TRACING_CONFIG_TEST, tracing_config_test));
    }

    if let Some(tracing_config) = &tracing_config {
        if !tracing_config.exists() {
            search_path.push((
                ENV_TRACING_CONFIG_PARENT,
                tracing_config.parent().map(|parent| parent.to_path_buf()),
            ));
        }
    }

    search_path.push((ENV_TRACING_CONFIG, tracing_config));
    search_path.push(("project_dirs_preference_dir", project_dirs_preference_dir));
    search_path.push(("project_dirs_config_dir", project_dirs_config_dir));
    search_path.push((
        "project_dirs_config_local_dir",
        project_dirs_config_local_dir,
    ));
    search_path.push(("base_dirs_preference_dir", base_dirs_preference_dir));
    search_path.push(("base_dirs_config_dir", base_dirs_config_dir));
    search_path.push(("base_dirs_config_local_dir", base_dirs_config_local_dir));
    search_path.push(("user_dirs_home_dir", user_dirs_home_dir));
    search_path.push(("base_dirs_home_dir", base_dirs_home_dir));
    search_path.push(("current_exe_dir", current_exe_dir));
    search_path.push(("current_dir", current_dir));

    let search_path: Vec<(&str, PathBuf)> = search_path
        .into_iter()
        .filter_map(|(name, path)| match path {
            None => {
                emit!(DEBUG, verbosity, "search_path : {name:<29} => not set");
                None
            }
            Some(path) => {
                let path_exists = path.exists();
                emit!(
                    DEBUG,
                    verbosity,
                    "search_path : {name:<29} => {} {}",
                    (if path_exists { "[v]" } else { "[ ]" }),
                    path.display()
                );
                if path_exists {
                    Some((name, path))
                } else {
                    None
                }
            }
        })
        .collect();

    for (name, path) in search_path {
        emit!(TRACE, verbosity, "checking path => {}", path.display());
        if path.is_file() {
            emit!(
                INFO,
                verbosity,
                "search_path : found {name} => {}",
                path.display()
            );
            return Some(path);
        }

        for config_file in &config_filenames {
            let config_path = path.join(config_file);
            emit!(
                TRACE,
                verbosity,
                "checking path => {}",
                config_path.display()
            );

            if config_path.is_file() {
                emit!(
                    INFO,
                    verbosity,
                    "search_path : found {name} => {}",
                    config_path.display()
                );
                return Some(config_path);
            }
        }
    }

    None
}

#[doc = include_str!("../doc/config_initialize.md")]
///
#[doc = include_str!("../doc/reference_links.md")]
pub fn initialize(
    qualifier: &str,
    organization: &str,
    name: &str,
    test: bool,
    verbosity: Option<model::Level>,
    env: Option<&str>,
    path: Option<&Path>,
    config: Option<model::TracingConfig>,
) -> Result<TracingConfigGuard, TracingConfigError> {
    static TRACING_INITIALIZED: Mutex<u32> = Mutex::new(0);

    let mut tracing_init_count = match TRACING_INITIALIZED.lock() {
        Ok(mtx_guard) => mtx_guard,
        Err(_poison) => {
            emit!(ERROR, verbosity, "Init lock is poisoned, this is a bug!");
            return Err(TracingConfigError::PoisonError(
                "TRACING_INITIALIZED".to_owned(),
            ));
        }
    };

    if *tracing_init_count > 0 {
        *tracing_init_count += 1;
        if !test {
            emit!(
                WARN,
                verbosity,
                "Ignored init, it must be called only once, usually in the main() function, init was called {} times",
                *tracing_init_count
            );
        }
        return Err(TracingConfigError::AlreadyInitialized);
    }

    fn init_config(
        verbosity: Option<model::Level>,
        tracing_config: model::TracingConfig,
    ) -> (bool, Result<TracingConfigGuard, TracingConfigError>) {
        let mut is_tracing_initialized = false;

        let (subscriber, guards) = match create_subscriber(verbosity, &tracing_config) {
            Ok(ok) => ok,
            Err(error) => {
                emit!(
                    ERROR,
                    verbosity,
                    "Could not create a subscriber : {error:#?}"
                );
                return (is_tracing_initialized, Err(error));
            }
        };

        match t::subscriber::set_global_default(subscriber) {
            Ok(_) => (),
            Err(error) => {
                emit!(
                    ERROR,
                    verbosity,
                    "Could not set subscriber as global default : {error:#?}"
                );
                return (
                    is_tracing_initialized,
                    Err(TracingConfigError::AlreadyInitialized),
                );
            }
        }

        is_tracing_initialized = true;

        emit!(INFO, verbosity, "Tracing successfully initialized!");

        (is_tracing_initialized, Ok(TracingConfigGuard::new(guards)))
    }

    if let Some(tracing_config) = config {
        emit!(
            INFO,
            verbosity,
            "Initializing with => config, title : \"{}\"",
            tracing_config.title
        );
        if path.is_some() {
            emit!(WARN, verbosity, "'config' is set, ignoring 'path'");
        }
        if env.is_some() {
            emit!(WARN, verbosity, "'config' is set, ignoring 'env'");
        }
        let (is_init, result_guard) = init_config(verbosity, tracing_config);
        *tracing_init_count += if is_init { 1 } else { 0 };
        return result_guard;
    }

    emit!(
        INFO,
        verbosity,
        "Initializing with => name : \"{}\", qualifier : \"{}\", organization : \"{}\", env = {:?}",
        name,
        qualifier,
        organization,
        env
    );

    let config_path = match find_config_path(
        qualifier,
        organization,
        name,
        test,
        verbosity,
        env,
        path,
    ) {
        Some(config_path) => config_path,
        None => {
            emit!(
                ERROR,
                verbosity,
                "Could not find the configuration file, please create a tracing.toml file and double check the 'tracing_config' env var"
            );
            return Err(TracingConfigError::ConfigFileNotFound);
        }
    };

    let tracing_config = match read_config(&config_path, RESOLVE_FROM_ENV_DEPTH) {
        Ok(tracing_config) => tracing_config,
        Err(read_error) => {
            emit!(
                ERROR,
                verbosity,
                "Could not read the config file. path = \"{}\"; error : {read_error:?}",
                config_path.display()
            );
            return Err(read_error);
        }
    };

    emit!(
        INFO,
        verbosity,
        "Loaded configuration file titled : '{}' from : {}",
        tracing_config.title,
        config_path.display()
    );

    let (is_init, result_guard) = init_config(verbosity, tracing_config);
    *tracing_init_count += if is_init { 1 } else { 0 };
    result_guard
}
