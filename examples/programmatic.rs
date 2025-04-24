use tracing::*;

fn main() {
    use common_macros::hash_map;
    use tracing_config::config::model;

    const STD_OUT_WRITER_NAME : &'static str = "stderr";
    const STD_ERR_WRITER_NAME : &'static str = "stderr";
    const FMT_LAYER_NAME : &'static str = "fmt_layer";
    const ROOT_FILTER_NAME: &'static str = "root";

    let config = model::TracingConfig {
        title: "Test init by config".to_owned(),
        writers: hash_map!(
            STD_OUT_WRITER_NAME.into() => model::Writer::StandardOutput,
            STD_ERR_WRITER_NAME.into() => model::Writer::StandardError
        ),
        layers: hash_map!(
            FMT_LAYER_NAME.into() => model::Layer::Fmt(model::FmtLayer {
                filter: None,
                writer: STD_ERR_WRITER_NAME.into(),
                formatter: model::FmtLayerFormatter::Compact,
                span_events: model::SpanEvents::None,
                ansi: true,
                time: Some(true),
                level: Some(true),
                target: Some(true),
                file: None,
                line_number: None,
                thread_ids: None,
                thread_names: None,
                span_list: None,
                current_span: Some(true),
                flatten_event: None,
            })
        ),
        filters: hash_map!(
            ROOT_FILTER_NAME.into() => model::Filter {
                level: model::Level::Trace,
                directives: None,
            }
        ),
    };

    tracing_config::init! {
        config : config,
    }

    let _main_info_span = info_span!("main").entered();
    info!("Hello World");

}