use std::convert::Into;
use std::path::Path;

use ::tracing as t;

use tracing_config::config::model::*;
use tracing_config::config::*;

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn get_test_conf_file() -> &'static Path {
    const TEST_CONF_FILE: &str = "";
    if TEST_CONF_FILE == "" {
        panic!("please set a value for the test conf file path in the test source");
    }
    std::path::Path::new(TEST_CONF_FILE)
}

#[test]
fn test_try_init_config() -> Result<()> {
    tracing_config::init!(
       path : get_test_conf_file()
    );

    let id = "my_id";

    let _e = t::info_span!("some_span", id).entered();
    t::info!("inside a span");

    Ok(())
}

#[test]
fn test_write() -> Result<()> {
    use std::collections::HashMap;

    let file_path = get_test_conf_file();

    let mut filters = HashMap::new();
    let mut writers = HashMap::new();
    let mut layers = HashMap::new();

    filters.insert(
        "trace".to_owned(),
        Filter {
            level: Level::Trace,
            directives: None,
        },
    );
    filters.insert(
        "root".to_owned(),
        Filter {
            level: Level::Trace,
            directives: None,
        },
    );

    writers.insert("stdout".to_owned(), Writer::StandardOutput);
    writers.insert(
        "proto".to_owned(),
        Writer::File(FileWriter {
            directory_path: "${env:fs_app_logs}/rust/proto".to_owned(),
            file_name: "${sl:id}".to_owned(),
            file_ext: Some("log".to_owned()),
            max_log_files: Some(50),
            rotation: Some(FileRotation::Daily),
            non_blocking: NonBlockingOptions {
                enabled: true,
                buffered_lines_limit: None,
                lossy: Some(false),
                thread_name: None,
            },
        }),
    );
    writers.insert(
        "util".to_owned(),
        Writer::File(FileWriter {
            directory_path: "${env:fs_app_logs}/rust/".to_owned(),
            file_name: "util".to_owned(),
            file_ext: None,
            max_log_files: None,
            rotation: Some(FileRotation::Daily),
            non_blocking: NonBlockingOptions {
                enabled: true,
                buffered_lines_limit: None,
                lossy: None,
                thread_name: None,
            },
        }),
    );
    writers.insert(
        "jayson".to_owned(),
        Writer::File(FileWriter {
            directory_path: "${env:fs_app_logs}/rust/".to_owned(),
            file_name: "util".to_owned(),
            file_ext: Some("json".to_owned()),
            max_log_files: None,
            rotation: Some(FileRotation::Daily),
            non_blocking: NonBlockingOptions {
                enabled: true,
                buffered_lines_limit: None,
                lossy: None,
                thread_name: None,
            },
        }),
    );

    layers.insert(
        "stdout".to_owned(),
        Layer::Fmt(FmtLayer {
            filter: Some("trace".to_owned()),
            writer: "stdout".to_owned(),
            ansi: true,
            span_events: SpanEvents::None,
            formatter: FmtLayerFormatter::Pretty,
            target: None,
            file: None,
            line_number: None,
            level: None,
            thread_ids: None,
            thread_names: None,
            time: None,
            span_list: None,
            current_span: None,
            flatten_event: None,
        }),
    );
    layers.insert(
        "proto-sifting".to_owned(),
        Layer::Sifting(SiftingLayer {
            filter: None,
            writer: "proto".to_owned(),
            layer: "proto".to_owned(),
            sift_on: vec!["id".to_owned()],
        }),
    );
    layers.insert(
        "proto".to_owned(),
        Layer::Fmt(FmtLayer {
            filter: None,
            writer: "${sl:sifted}".to_owned(),
            ansi: false,
            span_events: SpanEvents::Exit,
            formatter: FmtLayerFormatter::Full,
            target: None,
            file: None,
            line_number: None,
            level: None,
            thread_ids: None,
            thread_names: None,
            time: None,
            span_list: None,
            current_span: None,
            flatten_event: None,
        }),
    );
    layers.insert(
        "util".to_owned(),
        Layer::Fmt(FmtLayer {
            filter: None,
            writer: "util".to_owned(),
            ansi: false,
            span_events: SpanEvents::None,
            formatter: FmtLayerFormatter::Full,
            target: None,
            file: None,
            line_number: None,
            level: None,
            thread_ids: None,
            thread_names: None,
            time: None,
            span_list: None,
            current_span: None,
            flatten_event: None,
        }),
    );
    layers.insert(
        "jayson".to_owned(),
        Layer::Json(JsonLayer {
            filter: None,
            writer: "jayson".to_owned(),
            trailing_comma: None,
            pretty_json: Some(true),
            span_id: None,
            span_uuid: None,
            span_timestamp: None,
            span_level: None,
            span_name: None,
            span_target: None,
            span_module_path: None,
            span_file: None,
            span_line: None,
            span_fields: None,
            event_timestamp: None,
            event_level: None,
            event_name: None,
            event_target: None,
            event_module_path: None,
            event_file: None,
            event_line: None,
            event_fields: None,
            event_span_id: None,
            event_span_uuid: None,
            event_spans: None,
        }),
    );

    // Create a Config instance
    let config = TracingConfig {
        title: "Default Config".into(),
        filters,
        writers,
        layers,
    };

    write_config(&config, file_path)?;
    let deserialized_config: TracingConfig = read_config(file_path, RESOLVE_FROM_ENV_DEPTH)?;
    println!("Deserialized Config: {:#?}", deserialized_config);

    Ok(())
}

#[test]
fn test_read() -> Result<()> {
    let file_path = get_test_conf_file();

    let deserialized_config = read_config(file_path, RESOLVE_FROM_ENV_DEPTH)?;
    println!("Deserialized Config: {:#?}", deserialized_config);

    Ok(())
}
