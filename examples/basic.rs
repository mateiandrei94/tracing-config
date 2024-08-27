use tracing::*;
fn main() {
    tracing_config::init!(); // panics; read the docs on why and when.
    let _main_info_span = info_span!("main").entered();
    info!("Hello World");
}