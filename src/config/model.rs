//! This module contains the configuration file data model.
//!
//! All [`Option`][enum@std::option::Option] fields are optional and have default values.
//!
//! # Example detailed configuration file.
//!
//! ```toml
//! title = "My App tracing Config"
//! # declare a layer named "stdout"
//! [layer.stdout]
//! # type can be "fmt", "json", "sifting"
//! type = "fmt"
//! writer = "stdout" # in this configuration file there should be a writer called "stdout"
//! # the following are "fmt" layer specific properties
//! formatter = "pretty" # can be "full", "compact", "pretty", "json"
//! span_events = "none" # do not record span events
//! ansi = true # use ansi color escape codes
//! # the other properties are set to their default values
//!
//! # declare a writer named "stdout"
//! [writer.stdout]
//! type = "standard_output" # we only support "file" and "standard_output"
//! # standard_output writer does not require additional configuration
//!
//! # declare a layer named "util"
//! [layer.util]
//! type = "fmt"
//! writer = "util" # will write to "util" writer which is a file
//! # fmt layer specific config
//! formatter = "full"
//! span_events = "none"
//! ansi = false
//!
//! # declare a writer named "util"
//! [writer.util]
//! type = "file" # will write to a file
//! directory_path = "${env:fs_app_logs}/rust/" # ${env:fs_app_logs} there should be an environment variable named "fs_app_logs"
//! file_name = "util"
//! rotation = "daily" # every day we will get a new file in "directory_path"
//! non_blocking = true # does not block caller (when you tracing::info!(...) does not wait to write to the file)
//!
//! # declare a layer named "webapi"
//! [layer.webapi]
//! type = "sifting" # this is a sifting layer, it will dynamically create new layers and writers at runtime as new "sift_on" values are introduced
//! writer = "webapi" # the output will be the "webapi" writer which MUST have parameter values for each declared "sift_on" keys.
//! layer = "webapi-sifted" # what layer should be sifted ?
//! sift_on = ["id"] # the keys this sifting layers sifts.
//!
//! # declare a layer named "webapi-sifted"
//! # given that is is the target of a sifting layer it's writer must be ${sl:sifted}
//! # "sl" is a special placeholder replacement scheme indicating the initial letters of sifting layer
//! [layer.webapi-sifted]
//! type = "fmt"
//! writer = "${sl:sifted}"
//! formatter = "full"
//! span_events = "exit" # records exit span events
//! ansi = false # it records to a file, so no colors
//!
//! # declare a writer named "webapi"
//! # given that is is the target of a sifting layer it must incorporate all the keys defined in "sift_on" in the sifting layer
//! # in this case it's the id property, you will have a different file name for each different id
//! [writer.webapi]
//! type = "file"
//! directory_path = "${env:fs_app_logs}/rust/webapi"
//! file_name = "${sl:id}"
//! file_ext = "log"
//! max_log_files = 50 # keep at most 50 log files
//! rotation = "daily" # 50 log files daily means 50 days of history
//! non_blocking = true # async
//! lossy = false # no loss of information/logs
//!
//! # declare a layer named "jayson"
//! [layer.jayson]
//! type = "json" # uses the custom tracing-config json layer
//! writer = "jayson" # use the "jayson" writer
//! pretty = true
//!
//! # declare a writer named "jayson"
//! [writer.jayson]
//! type = "file"
//! directory_path = "${env:fs_app_logs}/rust/"
//! file_name = "jayson"
//! file_ext = "json"
//! rotation = "daily"
//! non_blocking = true
//!
//! # declare a filter named "my_filter"
//! [filter.my_filter]
//! level = "trace" # at lever trace
//! directives = [
//!     "my_module::proto=info" # however my_module::proto only emits info events and above
//! ]
//!
//! # this is required, it's the root filter
//! [filter.root]
//! level = "info" # if not specified otherwise the event should be at info level
//! directives = [
//!     # in this example we do not want events from the hyper crate at levels below error
//!     "hyper=error",
//!     "hyper::client::connect::dns=error",
//!     "hyper::proto::h1::conn=error",
//!     "hyper::proto::h1::conn=error",
//!     "hyper::proto::h1::io=error",
//!     "hyper::proto::h1::role=error",
//!     "hyper::proto::h1::encode=error",
//!     "hyper::client::pool=error",
//! ]
//! ```

use serde::{Deserialize, Serialize};

use std::collections::HashMap;

/// An [`fmt Layer`][trait@ts::layer::Layer] [`formatter (check docs)`][struct@ts::fmt::format::Format]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FmtLayerFormatter {
    Full,
    Compact,
    Pretty,
    Json,
}

/// A [`tracing`][mod@t] [`Level (check docs)`][struct@t::Level]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Level {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

/// A [`tracing appender`][mod@ta] [`file rotation (check docs)`][struct@ta::rolling::Rotation] scheme
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FileRotation {
    Minutely,
    Hourly,
    Daily,
    Never,
}

/// Span event types, check [`FmtSpan`][struct@ts::fmt::format::FmtSpan] Implementations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SpanEvents {
    /// one event when span is created
    New,
    /// one event per enter of a span
    Enter,
    /// one event per exit of a span
    Exit,
    /// one event when the span is dropped
    Close,
    /// spans are ignored (this is the default)
    None,
    /// one event per enter/exit of a span
    Active,
    /// events at all points (new, enter, exit, drop)
    Full,
}

/// A file [`Writer`][trait@ts::fmt::MakeWriter]
/// # Example
/// ```toml
/// # declare a writer named "my_file"
/// [writer.my_file]
/// type = "file"
/// directory_path = "${env:fs_app_logs}/rust/" # ${env:fs_app_logs} there should be an environment variable named "fs_app_logs"
/// file_name = "util"
/// # The following are optional
/// file_ext = "log" # filename will end in .log
/// max_log_files = 50 # keep at most 50 log files
/// rotation = "daily" # possible values are : "minutely", "hourly", "daily", "never"
/// # The following are optional and only related to non blocking
/// non_blocking = true # async
/// buffered_lines_limit = 2048 # lines buffer
/// lossy = false # no loss of information/logs
/// thread_name = "my_file_async_thread"
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FileWriter {
    /// Must point to an existing directory on the file system
    pub directory_path: String,
    /// The name of the tracing file without file extension
    pub file_name: String,
    /// The file extension without the . (dot)
    pub file_ext: Option<String>,
    /// The maximum number of log/tracing files inside of `directory_path` for this writer.
    /// Only valid if `FileRotation` is not `Never`
    pub max_log_files: Option<usize>,
    /// File rotation options
    pub rotation: Option<FileRotation>,
    /// Non blocking options
    #[serde(flatten)]
    pub non_blocking: NonBlockingOptions,
}

/// [`tracing appender`][mod@ta] [`NonBlocking`][struct@ta::non_blocking::NonBlockingBuilder] options
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct NonBlockingOptions {
    /// Whether non blocking is enabled or not
    #[serde(rename = "non_blocking")]
    pub enabled: bool,
    /// Number of lines to buffer before dropping logs (only valid if `lossy` is enabled)
    pub buffered_lines_limit: Option<usize>,
    /// If set to true, logs will be dropped when the buffered limit (`buffered_lines_limit`) is reached
    pub lossy: Option<bool>,
    /// Override the worker thread’s name.
    pub thread_name: Option<String>,
}

/// Supported [`Writer`][trait@ts::fmt::MakeWriter] types
/// # Example
/// ```toml
/// # declare a writer named "my_writer_stdout"
/// [writer.my_writer_stdout]
/// type = "standard_output"
/// # declare a writer named "my_file"
/// [writer.my_file]
/// type = "file"
/// # see FileWriter for FileWriter properties
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[serde(tag = "type")]
pub enum Writer {
    File(FileWriter),
    StandardOutput,
}

/// [`fmt Layer (see docs)`][struct@ts::fmt::Layer] configuration.
/// # Example
/// ```toml
/// # declare a layer named "app"
/// [layer.app]
/// type = "fmt"
/// writer = "my_file" # must be a declared writer
/// formatter =  "pretty" # can be : "full", "compact", "pretty", "json"
/// span_events = "none" # can be : "new", "enter", "exit", "close", "none", "active", "full"
/// ansi = false # color terminal
/// # the following properties are optional
/// filter = "only_app" # must be a named filter
/// time = true
/// level = true
/// target = true
/// file = true
/// line_number = true
/// thread_ids = true
/// thread_names = true
/// span_list = true # for "json" formatter only
/// current_span = true # for "json" formatter only
/// flatten_event = true # for "json" formatter only
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FmtLayer {
    /// Applies a per Layer filter, makes this a [`Filtered Layer`][struct@ts::filter::Filtered]
    pub filter: Option<String>,
    /// Where does the output go
    pub writer: String,
    /// Formatting kind/type
    pub formatter: FmtLayerFormatter,
    /// Which span events should be recorded
    pub span_events: SpanEvents,
    /// Ansi color escape codes (for color terminal) recommend to disable if writer is file
    pub ansi: bool,
    /// Record time
    pub time: Option<bool>,
    /// Record level
    pub level: Option<bool>,
    /// Record span/event target
    pub target: Option<bool>,
    /// Record source file
    pub file: Option<bool>,
    /// Record source file line number
    #[serde(rename = "line")]
    pub line_number: Option<bool>,
    /// Record thread id
    pub thread_ids: Option<bool>,
    /// Record thread name
    pub thread_names: Option<bool>,

    /// Record span list (json formatter only)
    #[serde(rename = "json_span_list")]
    pub span_list: Option<bool>,
    /// Record current span (json formatter only)
    #[serde(rename = "json_current_span")]
    pub current_span: Option<bool>,
    /// Flatten json output (json formatter only)
    #[serde(rename = "json_flatten_event")]
    pub flatten_event: Option<bool>,
}

/// tracing-config custom [`JsonLayer`][struct@crate::tracing::JsonLayer]
/// # Example
/// ```toml
/// # declare a layer named "app"
/// [layer.app]
/// type = "json"
/// writer = "my_file" # must be a declared writer
/// # the following properties are optional
/// filter = "only_app" # must be a named filter
/// trailing_comma = true
/// pretty_json = true
/// span_id = true
/// span_uuid = true
/// span_timestamp = true
/// span_level = true
/// span_name = true
/// span_target = true
/// span_module_path = true
/// span_file = true
/// span_line = true
/// span_fields = true
/// event_timestamp = true
/// event_level = true
/// event_name = true
/// event_target = true
/// event_module_path = true
/// event_file = true
/// event_line = true
/// event_fields = true
/// event_span_id = true
/// event_span_uuid = true
/// event_spans = true
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct JsonLayer {
    /// Applies a per Layer filter, makes this a [`Filtered Layer`][struct@ts::filter::Filtered]
    pub filter: Option<String>,
    /// Where does the output go
    pub writer: String,
    /// Add a comma after the json event (so you can parse the entire file as a JSON array)
    pub trailing_comma: Option<bool>,
    /// Pretty print events
    #[serde(rename = "pretty")]
    pub pretty_json: Option<bool>,
    /// Record span id
    pub span_id: Option<bool>,
    /// Record span uuid (a json layer concept only)
    pub span_uuid: Option<bool>,
    /// Record span timestamp
    pub span_timestamp: Option<bool>,
    /// Record span level
    pub span_level: Option<bool>,
    /// Record span name
    pub span_name: Option<bool>,
    /// Record span target
    pub span_target: Option<bool>,
    /// Record span module path
    pub span_module_path: Option<bool>,
    /// Record span source file
    pub span_file: Option<bool>,
    /// Record span source line
    pub span_line: Option<bool>,
    /// Record span fields (recorded fields)
    pub span_fields: Option<bool>,
    /// Record event timestamp
    pub event_timestamp: Option<bool>,
    /// Record event level
    pub event_level: Option<bool>,
    /// Record event name
    pub event_name: Option<bool>,
    /// Record event target
    pub event_target: Option<bool>,
    /// Record event module path
    pub event_module_path: Option<bool>,
    /// Record event source file
    pub event_file: Option<bool>,
    /// Record event source line
    pub event_line: Option<bool>,
    /// Record event fields (recorded fields)
    pub event_fields: Option<bool>,
    /// Record event span id
    pub event_span_id: Option<bool>,
    /// Record event span uuid (a json layer concept only)
    pub event_span_uuid: Option<bool>,
    /// Record event spans
    pub event_spans: Option<bool>,
}

/// tracing-config custom [`SiftingLayer (read how it works here)`][struct@crate::tracing::SiftingLayer].
///
/// Given it's purpose (i.e.: to dynamically create different layers based on some runtime criteria) the sifting layer will require a special writer as well as a special layer to sift on.
///
/// > **IMPORTANT** : The sifting layer will create (dynamically at runtime) a new `layer` for each set of different `sift_on` values.
/// >
/// > For example; to handle a user, the application enters a "user_handler" info span saving username as span data `info_span!("user_handler", username)`.
/// >
/// > While it does not make sense to create drastically different layers for each different user, having the same layer (or a copy or clone of it) write to a different file does (and it's the primary goal of the sifting layer).
/// > In the example above the `sift_on` value should be `sift_on = ["username"]`.
///
/// # Limitation
/// The limitation of the configuration file form of the sifting layer is the fact that it will essentially clone the sifted layer with a different writer.
/// This means that the writer **MUST** be different for each set of values declared in the `sift_on` field.
///
/// # Selector
/// This represents a [`SiftingLayerSelector`][struct@crate::tracing::SiftingLayerSelector].
///
/// To select for metadata :
/// - `${meta:level}` => selects level
/// - `${meta:name}` => selects name
/// - `${meta:target}` => selects target
/// - `${meta:module_path}` => selects module_path
/// - `${meta:file}` => selects file
/// - `${meta:line}` => selects line
///
/// Otherwise simply type in the name of the span data key as seen in the source code.
/// ```toml
/// sift_on = ["username", "${meta:module_path}"]
/// ```
///
/// # Writer
/// The configuration file form of the sifting layer only supports the [`FileWriter`][struct@FileWriter].
///
/// A sifting layer writer has additional/special placeholders in the form of `${sl:key}` and `${sl:meta:}` available as values in the configuration file.
/// These additional placeholders can be used in the following properties of the file writer :
/// - `directory_path`
/// - `file_name`
/// - `file_ext`
///
/// Here is a list of the available meta values :
/// - `${sl:meta:level}` => returns the level of the event
/// - `${sl:meta:name}` => returns the name of the event
/// - `${sl:meta:target}` => returns the target of the event
/// - `${sl:meta:module_path}` => returns the module path of the event
/// - `${sl:meta:file}` => returns the source file of the event
/// - `${sl:meta:line}` => returns the source line of the event
///
/// For any other `sift_on` non meta value the syntax is `${sl:key}` the word `meta` itself is allowed as a non meta key.
/// For example `info_span!("metadata_handler", meta)` use `sift_on = ["meta"]` and `${sl:meta}` to retrieve the value of meta.
///
/// > **IMPORTANT** : All values declared in the `sift_on` field must be used in either `directory_path`, `file_name` or `file_ext`.
/// > If meta values are required they must also be declared in the `sift_on` list.
/// >
/// > Example :
/// > ```toml
/// > # sifting layer
/// > sift_on = ["username", "${meta:level}"]
/// > # later on file writer
/// > directory_path = "${env:fs_app_logs}/rust/webapi/${sl:username}"
/// > file_ext = "${sl:meta:level}.log"
/// > # Notice we used both username and level
/// > ```
///
/// # Layer
/// The configuration file form of the sifting layer can only sift trough the [`FmtLayer`][struct@FmtLayer] or the [`JsonLayer`][struct@JsonLayer].
/// The programmatic form of the [`SiftingLayer`][struct@crate::tracing::SiftingLayer] can sift trough any type of [`Layer`][trait@ts::Layer].
///
/// The layer must **NOT** contain a `filter` itself. This is because the sifting layer creates fake layers and does not register them with tracing thus the filters (if any) are not registered either.
/// A filter on a sifted layer results in a panic at runtime.
///
/// The writer of a sifted layer must be `${sl:sifted}` otherwise it will be processed as a regular layer.
///
/// Example :
/// ```toml
/// [layer.webapi-sifted]
/// # a sifted layer MUST NOT include a filter
/// type = "fmt"
/// writer = "${sl:sifted}" # the writer MUST be the sifted writer
/// ```
///
/// # Example
/// ```toml
/// # declare a layer named "webapi"
/// [layer.webapi]
/// type = "sifting" # this is a sifting layer, it will dynamically create new layers and writers at runtime as new "sift_on" values are introduced
/// writer = "webapi_writer" # the output will be the "webapi" writer which MUST have parameter values for each declared "sift_on" keys.
/// layer = "webapi-sifted" # what layer should be sifted ? must NOT have a filter
/// sift_on = ["id"] # the keys this sifting layers sifts.
///
/// # we must declare a writer that includes all "sift_on" keys named "webapi_writer"
/// [writer.webapi_writer]
/// type = "file"
/// directory_path = "${env:fs_app_logs}/rust/webapi"
/// file_name = "${sl:id}" # use the "sl" scheme to refer back to "sift_on" keys
/// file_ext = "log"
/// max_log_files = 50 # keep at most 50 log files
/// rotation = "daily" # 50 log files daily means 50 days of history
/// non_blocking = true # async
/// lossy = false # no loss of information/logs
///
/// # we must also declare the sifted layer, you can name it however you want, i chose to add "-sifted" to the original name
/// [layer.webapi-sifted]
/// # a sifted layer MUST NOT include a filter
/// type = "fmt"
/// writer = "${sl:sifted}" # the writer MUST be the sifted writer
/// formatter = "full"
/// span_events = "exit" # records exit span events
/// ansi = false # it records to a file, so no colors
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SiftingLayer {
    /// Applies a per Layer filter, makes this a [`Filtered Layer`][struct@ts::filter::Filtered]
    pub filter: Option<String>,
    /// A supported sifting writer; documented above.
    pub writer: String,
    /// A supported sifted layer; documented above. The layer must **NOT** contain a `filter` itself.
    pub layer: String,
    /// This represents a [`SiftingLayerSelector`][struct@crate::tracing::SiftingLayerSelector].
    pub sift_on: Vec<String>,
}

/// Supported [`Layer`][trait@ts::Layer] types
/// # Example
/// ```toml
/// # declare a layer named "app"
/// [layer.app]
/// type = "fmt" # type can be "fmt", "json", "sifting"
/// # all other properties depend on the type of layer you are configuring
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[serde(tag = "type")]
pub enum Layer {
    Fmt(FmtLayer),
    Json(JsonLayer),
    Sifting(SiftingLayer),
}

/// A [`Filter`][trait@ts::layer::Filter]
///
/// A filter named `root` is required in your configuration file
///
/// ```toml
/// [filter.root]
/// level = "debug"
/// directives = []
/// ```
///
/// # Example
/// ```toml
/// # declare a filter named "my_filter"
/// [filter.my_filter]
/// level = "trace" # at lever trace, possible values are : "trace", "debug", "info", "warn", "error"
/// directives = [ # see tracing subscriber env filter for more details
///     "my_module::proto=info" # however my_module::proto only emits info events and above
/// ]
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Filter {
    /// Base filter level
    pub level: Level,
    /// See docs at [`EnvFilter`][struct@ts::filter::EnvFilter] `Directives` section
    pub directives: Option<Vec<String>>,
}

/// This represents the whole configuration file.
/// - `title` : does not do anything, is is for your reference
/// - `writers` : list all your writers (where tracing will write events), only 1 per layer is allowed, reuse is not allowed
/// - `layers` : list all your layers, these will all be added to a [`Registry`][struct@ts::registry::Registry]
/// - `filters` : list all your filters by name, reuse is allowed
///
/// # Example :
/// ```toml
/// title = "My App tracing Config"
///
/// [writer.my_output]
/// # writer properties
///
/// [writer.second]
/// # writer properties
///
/// [layer.one]
/// # layer properties
///
/// [layer.two]
/// # layer properties
///
/// [filter.f_one]
/// # filter properties
///
/// [filter.f_two]
/// # filter properties
///
/// # this is required, it's the "root" filter
/// [filter.root]
/// # filter properties
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TracingConfig {
    pub title: String,
    #[serde(rename = "writer")]
    pub writers: HashMap<String, Writer>,
    #[serde(rename = "layer")]
    pub layers: HashMap<String, Layer>,
    #[serde(rename = "filter")]
    pub filters: HashMap<String, Filter>,
}
