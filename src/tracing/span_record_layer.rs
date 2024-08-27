use std::fmt::Debug;

use chrono::{DateTime, Local as DateTimeLocal};

use serde_json::json;
use serde_json::value::Value as JsonValue;

use indexmap::IndexMap;
use uuid::Uuid;

use t::Subscriber;
use t::span::{Span, Attributes as SpanAttributes, Id as SpanId, Record as TSpanRecord};
use t::field::{Field as TField, Visit as TFieldVisit};

use ts::layer::{Context, Layer as TsLayer};
use ts::registry::{Registry, SpanRef, LookupSpan};

/// A simple data structure that represents span data in JSON format.
///
/// This only words if the [`tracing`][mod@t] [`Subscriber`][trait@t::Subscriber] is a [`Registry`][struct@ts::registry::Registry] and the [`SpanRecordLayer`][struct@Layer] is attached to it right after the primary filter.
///
/// - `timestamp` : when the span was created
/// - `uuid` : generated (useful for correlating events)
/// - `map` : key-value ordered map of the fields recorded in the span
pub struct SpanRecord {
    pub timestamp: DateTime<DateTimeLocal>,
    pub uuid: Uuid,
    pub map: IndexMap<&'static str, JsonValue>,
}

impl SpanRecord {
    pub fn new() -> Self {
        SpanRecord {
            timestamp: DateTimeLocal::now(),
            uuid: Uuid::new_v4(),
            map: IndexMap::new(),
        }
    }
}

impl Default for SpanRecord {
    fn default() -> Self {
        Self::new()
    }
}

impl TFieldVisit for SpanRecord {
    fn record_debug(&mut self, field: &TField, value: &dyn Debug) {
        self.map.insert(field.name(), json!(format!("{:?}", value)));
    }

    fn record_str(&mut self, field: &TField, value: &str) {
        self.map.insert(field.name(), json!(value));
    }

    fn record_bool(&mut self, field: &TField, value: bool) {
        self.map.insert(field.name(), json!(value));
    }

    fn record_u64(&mut self, field: &TField, value: u64) {
        self.map.insert(field.name(), json!(value));
    }

    fn record_i64(&mut self, field: &TField, value: i64) {
        self.map.insert(field.name(), json!(value));
    }

    fn record_f64(&mut self, field: &TField, value: f64) {
        self.map.insert(field.name(), json!(value));
    }

    fn record_i128(&mut self, field: &TField, value: i128) {
        self.map.insert(field.name(), json!(value));
    }

    fn record_u128(&mut self, field: &TField, value: u128) {
        self.map.insert(field.name(), json!(value));
    }

    fn record_error(&mut self, field: &TField, value: &(dyn std::error::Error + 'static)) {
        self.map.insert(
            field.name(),
            json!({
                "display" : format!("{}", value),
                "debug" : format!("{:?}", value)
            }),
        );
    }
}

/// Adds a tracing span extension to every span attaching a [`SpanRecord`] structure.
pub struct Layer {
    _marker: std::marker::PhantomData<String>,
}

/// Adds a [`SpanRecord`][struct@SpanRecord] to every span as `extensions`.
///
/// See [`SpanData`][trait@ts::registry::SpanData] and [`Extensions`][struct@ts::registry::Extensions].
impl Layer {
    pub fn new() -> Self {
        Self {
            _marker: std::marker::PhantomData {},
        }
    }
}

impl Default for Layer {
    fn default() -> Self {
        Self::new()
    }
}

impl<S> TsLayer<S> for Layer
where
    S: Subscriber,
    S: for<'lookup> LookupSpan<'lookup>,
{
    fn on_new_span(&self, attributes: &SpanAttributes<'_>, id: &SpanId, context: Context<'_, S>) {
        if let Some(span) = context.span(id) {
            let mut span_extensions = span.extensions_mut();
            if let Some(span_values) = span_extensions.get_mut::<SpanRecord>() {
                attributes.record(span_values);
            } else {
                let mut span_values = SpanRecord::new();
                attributes.record(&mut span_values);
                span_extensions.insert(span_values);
            }
        }
    }

    // todo : on_enter() ---- update idle time (time from on_new_span() to now)
    // todo : on_exit() ---- update busy time (time from on_enter() to now)
    // todo : on_close() ---- update idle time (time from on_exit() to now)

    fn on_record(&self, id: &SpanId, attributes: &TSpanRecord<'_>, context: Context<'_, S>) {
        if let Some(span) = context.span(id) {
            let mut span_extensions = span.extensions_mut();
            if let Some(span_values) = span_extensions.get_mut::<SpanRecord>() {
                attributes.record(span_values);
            } else {
                let mut span_values = SpanRecord::new();
                attributes.record(&mut span_values);
                span_extensions.insert(span_values);
            }
        }
    }
}

impl SpanRecord {
    /// Returns the first span value starting from the current span matching `key`, this inspects span parents as well.
    pub fn span_value(key: &str) -> Option<JsonValue> {
        Self::get_current_span_data(true, |map| {
            let value = map.get(key);
            if let Some(value) = value {
                return Some(value.clone());
            }
            None
        })
    }

    /// Returns the first `Some(value)` produced by the closure `f`, or `None` if no value is found.
    ///
    /// The closure `f` is initially called with the current span's key-value map.
    /// If `parents` is true and `f` previously returned `None`, it will be called again with the span's parent map.
    /// This continues until `f` returns `Some(value)`, otherwise the function returns `None`.
    pub fn get_current_span_data<F, R>(parents: bool, f: F) -> Option<R>
    where
        F: FnMut(&IndexMap<&'static str, JsonValue>) -> Option<R>,
    {
        Self::get_span_data(parents, &Span::current(), f)
    }

    /// Returns the first `Some(value)` produced by the closure `f`, or `None` if no value is found.
    ///
    /// The closure `f` is initially called with the parameter `span` span's key-value map.
    /// If `parents` is true and `f` previously returned `None`, it will be called again with the span's parent map.
    /// This continues until `f` returns `Some(value)`, otherwise the function returns `None`.
    pub fn get_span_data<F, R>(parents: bool, span: &Span, f: F) -> Option<R>
    where
        F: FnMut(&IndexMap<&'static str, JsonValue>) -> Option<R>,
    {
        span.with_subscriber(|(id, dispatch)| {
            if let Some(registry) = dispatch.downcast_ref::<Registry>() {
                if let Some(span) = registry.span(id) {
                    Self::get_tracing_span_data(parents, &span, f)
                } else {
                    None
                }
            } else {
                None
            }
        })?
    }

    /// Given a tracing span, loops trough all the span data by calling `f`
    fn get_tracing_span_data<F, R>(
        parents: bool,
        span: &SpanRef<'_, Registry>,
        mut f: F,
    ) -> Option<R>
    where
        F: FnMut(&IndexMap<&'static str, JsonValue>) -> Option<R>,
    {
        let span_extensions = span.extensions();
        if let Some(val) = span_extensions.get::<SpanRecord>() {
            let val = f(&val.map);
            if val.is_some() {
                return val;
            }
            if !parents {
                return None;
            }
            if let Some(span_parent) = span.parent() {
                Self::get_tracing_span_data(parents, &span_parent, f)
            } else {
                None
            }
        } else {
            None
        }
    }
}
