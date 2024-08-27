use std::borrow::BorrowMut as _;
use std::io::Write as _;

use chrono::Local as DateTimeLocal;

use serde_json::json;
use serde_json::Map as JsonMap;
use serde_json::value::{Value as JsonValue, Value::Null as JsonNull};

use t::{Subscriber, Metadata, Event as TEvent};
use t::span::Id as SpanId;

use ts::layer::{Context, Layer as TsLayer};
use ts::registry::{SpanRef, LookupSpan};
use ts::fmt::MakeWriter;

use crate::tracing::SpanRecord;

/// A [`tracing-subscriber`][mod@ts] [`Layer`][trait@TsLayer] that outputs json to a [`MakeWriter`][trait@MakeWriter].
pub struct Layer<W> {
    make_writer: W,
    with_pretty_json: bool,
    with_trailing_comma: bool,
    with_span_id: bool,
    with_span_uuid: bool,
    with_span_timestamp: bool,
    with_span_level: bool,
    with_span_name: bool,
    with_span_target: bool,
    with_span_module_path: bool,
    with_span_file: bool,
    with_span_line: bool,
    with_span_fields: bool,
    with_event_timestamp: bool,
    with_event_level: bool,
    with_event_name: bool,
    with_event_target: bool,
    with_event_module_path: bool,
    with_event_file: bool,
    with_event_line: bool,
    with_event_fields: bool,
    with_event_span_id: bool,
    with_event_span_uuid: bool,
    with_event_spans: bool,
}

impl<W> Layer<W> {
    pub fn new(w: W) -> Self {
        Self {
            make_writer: w,
            with_pretty_json: false,
            with_trailing_comma: false,
            with_span_id: false,
            with_span_uuid: true,
            with_span_timestamp: false,
            with_span_level: true,
            with_span_name: true,
            with_span_target: true,
            with_span_module_path: false,
            with_span_file: true,
            with_span_line: true,
            with_span_fields: true,
            with_event_timestamp: true,
            with_event_level: true,
            with_event_name: true,
            with_event_target: true,
            with_event_module_path: false,
            with_event_file: false,
            with_event_line: false,
            with_event_fields: true,
            with_event_span_id: true,
            with_event_span_uuid: true,
            with_event_spans: true,
        }
    }

    pub fn with_pretty_json(mut self, value: bool) -> Self {
        self.with_pretty_json = value;
        self
    }

    pub fn with_trailing_comma(mut self, value: bool) -> Self {
        self.with_trailing_comma = value;
        self
    }

    pub fn with_span_id(mut self, value: bool) -> Self {
        self.with_span_id = value;
        self
    }

    pub fn with_span_uuid(mut self, value: bool) -> Self {
        self.with_span_uuid = value;
        self
    }

    pub fn with_span_timestamp(mut self, value: bool) -> Self {
        self.with_span_timestamp = value;
        self
    }

    pub fn with_span_level(mut self, value: bool) -> Self {
        self.with_span_level = value;
        self
    }

    pub fn with_span_name(mut self, value: bool) -> Self {
        self.with_span_name = value;
        self
    }

    pub fn with_span_target(mut self, value: bool) -> Self {
        self.with_span_target = value;
        self
    }

    pub fn with_span_module_path(mut self, value: bool) -> Self {
        self.with_span_module_path = value;
        self
    }

    pub fn with_span_file(mut self, value: bool) -> Self {
        self.with_span_file = value;
        self
    }

    pub fn with_span_line(mut self, value: bool) -> Self {
        self.with_span_line = value;
        self
    }

    pub fn with_span_fields(mut self, value: bool) -> Self {
        self.with_span_fields = value;
        self
    }

    pub fn with_event_timestamp(mut self, value: bool) -> Self {
        self.with_event_timestamp = value;
        self
    }

    pub fn with_event_level(mut self, value: bool) -> Self {
        self.with_event_level = value;
        self
    }

    pub fn with_event_name(mut self, value: bool) -> Self {
        self.with_event_name = value;
        self
    }

    pub fn with_event_target(mut self, value: bool) -> Self {
        self.with_event_target = value;
        self
    }

    pub fn with_event_module_path(mut self, value: bool) -> Self {
        self.with_event_module_path = value;
        self
    }

    pub fn with_event_file(mut self, value: bool) -> Self {
        self.with_event_file = value;
        self
    }

    pub fn with_event_line(mut self, value: bool) -> Self {
        self.with_event_line = value;
        self
    }

    pub fn with_event_fields(mut self, value: bool) -> Self {
        self.with_event_fields = value;
        self
    }

    pub fn with_event_span_id(mut self, value: bool) -> Self {
        self.with_event_span_id = value;
        self
    }

    pub fn with_event_span_uuid(mut self, value: bool) -> Self {
        self.with_event_span_uuid = value;
        self
    }

    pub fn with_event_spans(mut self, value: bool) -> Self {
        self.with_event_spans = value;
        self
    }
}

impl<W> Layer<W>
where
    W: for<'writer> MakeWriter<'writer> + 'static,
{
    fn span_to_json<S>(&self, span: &SpanRef<'_, S>) -> JsonValue
    where
        S: Subscriber,
        S: for<'lookup> LookupSpan<'lookup>,
    {
        let span_extensions = span.extensions();
        let span_values = span_extensions
            .get::<SpanRecord>()
            .expect("You need to register the SpanValues layer first");
        let metadata = span.metadata();

        let mut json = JsonMap::new();
        // for the inserts here i initially wanted to use serde_json::to_value()
        // however it requires handling a result
        if self.with_span_id {
            json.insert("id".to_owned(), json!(span.id().into_u64()));
        }
        if self.with_span_uuid {
            json.insert("uuid".to_owned(), json!(&span_values.uuid));
        }
        if self.with_span_timestamp {
            json.insert("timestamp".to_owned(), json!(&span_values.timestamp));
        }
        if self.with_span_level {
            json.insert("level".to_owned(), json!(metadata.level().as_str()));
        }
        if self.with_span_name {
            json.insert("name".to_owned(), json!(metadata.name()));
        }
        if self.with_span_target {
            json.insert("target".to_owned(), json!(metadata.target()));
        }
        if self.with_span_module_path {
            json.insert("module_path".to_owned(), json!(metadata.module_path()));
        }
        if self.with_span_file {
            json.insert("file".to_owned(), json!(metadata.file()));
        }
        if self.with_span_line {
            json.insert("line".to_owned(), json!(metadata.line()));
        }
        if self.with_span_fields {
            json.insert("fields".to_owned(), json!(&span_values.map));
        }

        json!(json)
    }

    fn json_write(&self, json_value: &JsonValue, metadata: &Metadata) {
        let mut writer = self.make_writer.make_writer_for(metadata);
        let write_result = if self.with_pretty_json {
            serde_json::to_writer_pretty(writer.borrow_mut(), json_value)
        } else {
            serde_json::to_writer(writer.borrow_mut(), json_value)
        };

        match write_result {
            Ok(ok) => ok,
            Err(err) => {
                let now = DateTimeLocal::now();
                let error =
                    format!("{now} -> ERROR : JsonLayer -- json serialization error ! --> {err:?}");
                println!("{error}");
                eprintln!("{error}");
            }
        }

        let write_result = writeln!(
            writer,
            "{}",
            if self.with_trailing_comma { "," } else { "" }
        );

        match write_result {
            Ok(ok) => ok,
            Err(err) => {
                let now = DateTimeLocal::now();
                let error =
                    format!("{now} -> ERROR : JsonLayer -- new_line write error ! --> {err:?}");
                println!("{error}");
                eprintln!("{error}");
            }
        }
    }
}

impl<S, W> TsLayer<S> for Layer<W>
where
    S: Subscriber,
    S: for<'lookup> LookupSpan<'lookup>,
    W: for<'writer> MakeWriter<'writer> + 'static,
{
    // fn on_new_span(&self, _attrs: &span::Attributes<'_>, _id: &span::Id, _ctx: Context<'_, S>) {}

    // fn on_record(&self, _span: &span::Id, _values: &span::Record<'_>, _ctx: Context<'_, S>) {}

    fn on_enter(&self, _id: &SpanId, _ctx: Context<'_, S>) {
        // if let Some(span) = ctx.span(id) {
        //     if let Some(scope) = ctx.span_scope(id) {
        //         let meta = span.metadata();
        //         let mut spans = vec![];

        //         for span in scope.from_root() { // scope or scope.from_root()
        //             spans.push(self.span_to_json(&span));
        //         }
        //         let output = serde_json::json!({
        //             "spans": spans,
        //         });

        //         self.json_write(&output, &meta);
        //     }
        // }
    }

    // fn on_exit(&self, _id: &span::Id, _ctx: Context<'_, S>) {}

    // fn on_close(&self, _id: span::Id, _ctx: Context<'_, S>) {}

    fn on_event(&self, event: &TEvent<'_>, ctx: Context<'_, S>) {
        let timestamp = DateTimeLocal::now();

        let event_span = ctx.event_span(event);

        let spans = match ctx.event_scope(event) {
            None => None,
            Some(scope) => {
                let mut spans = vec![];
                for span in scope.from_root() {
                    spans.push(self.span_to_json(&span));
                }
                Some(spans)
            }
        };

        let metadata = event.metadata();
        let level = metadata.level();
        let name = metadata.name();
        let target = metadata.target(); // Typically, this is the module path
        let module_path = metadata.module_path();
        let file = metadata.file();
        let line = metadata.line();

        // The fields of the event
        let mut fields = SpanRecord::new();
        event.record(&mut fields);

        let mut json = JsonMap::new();
        // for the inserts here i initially wanted to use serde_json::to_value()
        // however it requires handling a result
        if self.with_event_timestamp {
            json.insert("timestamp".to_owned(), json!(timestamp));
        }
        if self.with_event_level {
            json.insert("level".to_owned(), json!(level.as_str()));
        }
        if self.with_event_name {
            json.insert("name".to_owned(), json!(name));
        }
        if self.with_event_target {
            json.insert("target".to_owned(), json!(target));
        }
        if self.with_event_module_path {
            json.insert("module_path".to_owned(), json!(module_path));
        }
        if self.with_event_file {
            json.insert("file".to_owned(), json!(file));
        }
        if self.with_event_line {
            json.insert("line".to_owned(), json!(line));
        }
        if self.with_event_fields {
            json.insert("fields".to_owned(), json!(fields.map));
        }
        if self.with_event_span_id || self.with_event_span_uuid {
            if let Some(event_span) = event_span {
                let mut span_json = JsonMap::new();
                let span_extensions = event_span.extensions();
                let span_values = span_extensions
                    .get::<SpanRecord>()
                    .expect("You need to register the SpanValues layer first");
                if self.with_event_span_id {
                    span_json.insert("id".to_owned(), json!(event_span.id().into_u64()));
                }
                if self.with_event_span_uuid {
                    span_json.insert("uuid".to_owned(), json!(&span_values.uuid));
                }
                json.insert("span".to_owned(), json!(span_json));
            } else {
                json.insert("span".to_owned(), JsonNull);
            }
        }
        if self.with_event_spans {
            json.insert("spans".to_owned(), json!(spans));
        }

        // And create our output
        let output = json!(json);

        self.json_write(&output, event.metadata());
    }
}
