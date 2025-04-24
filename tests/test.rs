mod config;
mod interpolate;

use std::collections::HashMap;
use std::path::Path;
use std::env;
use tracing::*;
use tracing_config::init;
use tracing_config::config::model;

static ENV_TRACING_CONFIG: &str = "tracing_config";
static ENV_TRACING_CONFIG_TEST: &str = "tracing_config_test";
static ENV_TRACING_CONFIG_VERBOSITY: &str = "tracing_config_verbosity";
static ENV_CUSTOM: &str = "my_var";

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn reset_vars() {
    unsafe {
        env::remove_var(ENV_TRACING_CONFIG);
        env::remove_var(ENV_TRACING_CONFIG_TEST);
        env::remove_var(ENV_TRACING_CONFIG_VERBOSITY);
    }
}

// Please do these tests manually and monitor the standard output.
// I have no idea how to automate these "init" tests.
// Whether good or bad, these tests should pass "automatically"
// Yes, I know, env::set_var / remove_var is unsafe, convenient in these tests though.

/// Test init! verbosity by env
#[test]
#[should_panic]
fn test_verbosity_by_env() {
    reset_vars();
    unsafe {
        env::set_var(ENV_TRACING_CONFIG_VERBOSITY, "debug");
    }
    init!();
}

/// Test init! by providing a model::TracingConfig
#[test]
fn test_init_by_config() -> Result<()> {
    reset_vars();

    struct NameValue<T> {
        name: &'static str,
        value: T,
    }

    let mut writers = HashMap::new();
    let mut layers = HashMap::new();
    let mut filters = HashMap::new();

    let std_out_writer = NameValue {
        name: "stdout",
        value: model::Writer::StandardOutput,
    };
    writers.insert(std_out_writer.name.to_owned(), std_out_writer.value);

    let fmt_layer = NameValue {
        name: "fmt_layer",
        value: model::Layer::Fmt(model::FmtLayer {
            filter: None,
            writer: std_out_writer.name.to_owned(),
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
        }),
    };
    layers.insert(fmt_layer.name.to_owned(), fmt_layer.value);

    let root_filter = NameValue {
        name: "root",
        value: model::Filter {
            level: model::Level::Trace,
            directives: None,
        },
    };
    filters.insert(root_filter.name.to_owned(), root_filter.value);

    let config = model::TracingConfig {
        title: "test init by config".to_owned(),
        writers,
        layers,
        filters,
    };

    let ignored_path = Path::new("/this/should/be/ignored");
    let ignored_env = ENV_CUSTOM;

    init! {
        config : config,
        path : ignored_path,
        env : ignored_env,
        verbosity : "trace",
    }

    let _span = info_span!("my_span").entered();
    info!("Test done!");

    Ok(())
}

/// Test init! by providing a Path to a valid file
#[test]
fn test_init_by_path_file() -> Result<()> {
    reset_vars();

    let manifest_dir = env!("CARGO_MANIFEST_DIR");

    let path = Path::new(manifest_dir).join("tests/tracing-custom.toml");
    let ignored_env = ENV_CUSTOM;

    init! {
        path : path.as_path(),
        env : ignored_env,
        verbosity : "trace",
    }

    let _span = info_span!("my_span").entered();
    info!("Test done!");

    Ok(())
}

/// Test init! by providing a custom env to a valid file
#[test]
fn test_init_by_env_file() -> Result<()> {
    reset_vars();

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir).join("tests/tracing-custom.toml");
    let wrong_path = path.join("does_not_exist");
    let env = ENV_CUSTOM;

    unsafe {
        env::set_var(ENV_CUSTOM, path.to_str().unwrap());
    }

    init! {
        path : wrong_path.as_path(),
        env : env,
        verbosity : "trace",
    }

    let _span = info_span!("my_span").entered();
    info!("Test done!");

    Ok(())
}

/// Test init! by providing a custom name
#[test]
fn test_init_custom_name() -> Result<()> {
    reset_vars();

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir).join("examples");
    let wrong_path = path.join("does_not_exist");
    let _env = ENV_CUSTOM;

    unsafe {
        env::set_var(ENV_CUSTOM, wrong_path.to_str().unwrap());
        env::set_var(ENV_TRACING_CONFIG, path.to_str().unwrap());
    }

    init! {
        path : wrong_path.as_path(),
        // env : _env, // uncomment this and it should find env_parent (has priority over default)
        name : "custom",
        test : false,
        verbosity : "trace",
    }

    let _span = info_span!("my_span").entered();
    info!("Test done!");

    Ok(())
}

/// Test init! by providing an invalid toml file
#[test]
#[should_panic]
fn test_init_non_toml() -> () {
    reset_vars();

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir).join("tests/tracing-garbage.toml");

    unsafe {
        env::set_var(ENV_TRACING_CONFIG, path.to_str().unwrap());
    }

    init! {
        name : "garbage",
        test : false,
        verbosity : "trace",
    }

    let _span = info_span!("my_span").entered();
    info!("Test done!");

    ()
}

/// Test init! by calling it multiple times
#[test]
fn test_init_multi() -> () {
    reset_vars();

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir).join("examples");

    unsafe {
        env::set_var(ENV_TRACING_CONFIG, path.to_str().unwrap());
    }

    init! {
        name : "custom",
        test : false,
        verbosity : "trace",
    }

    init!();
    init!();
    init!();

    let _span = info_span!("my_span").entered();
    info!("Test done!");

    ()
}

/// Test init! by using a different init library
#[test]
#[should_panic]
fn test_init_other_lib() -> () {
    reset_vars();

    // different init library here
    tracing_subscriber::fmt().init();

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir).join("examples");

    unsafe {
        env::set_var(ENV_TRACING_CONFIG, path.to_str().unwrap());
    }

    init! {
        name : "custom",
        test : false,
        verbosity : "trace",
    }

    let _span = info_span!("my_span").entered();
    info!("Test done!");

    ()
}

/// Test init! no root filter
#[test]
#[should_panic]
fn test_init_missing_root_filter() -> () {
    reset_vars();

    // different init library here
    tracing_subscriber::fmt().init();

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir).join("tests/missing_root_filter.toml");

    unsafe {
        env::set_var(ENV_TRACING_CONFIG, path.to_str().unwrap());
    }

    init! {
        name : "custom",
        test : false,
        verbosity : "trace",
    }

    let _span = info_span!("my_span").entered();
    info!("Test done!");

    ()
}

/// Test init! env var reference
#[test]
fn test_init_normal_ref_var() -> Result<()> {
    reset_vars();

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir).join("examples");

    unsafe {
        env::set_var(ENV_TRACING_CONFIG, "${env:ref_one}");
        env::set_var("ref_one", "${env:ref_two}");
        env::set_var("ref_two", "${env:ref_three}");
        env::set_var("ref_three", "${env:ref_four}");
        env::set_var("ref_four", path.to_str().unwrap());
    }

    init! {
        name : "custom",
        test : false,
        verbosity : "trace",
    }

    let _span = info_span!("my_span").entered();
    info!("Test done!");

    Ok(())
}

/// Test init! env var reference, not found
#[test]
fn test_init_normal_ref_not_found() -> Result<()> {
    reset_vars();

    unsafe {
        env::set_var(ENV_TRACING_CONFIG, "${env:ref_one}");
        env::set_var("ref_one", "${env:ref_two}");
        env::set_var("ref_two", "${env:ref_three}");
        env::set_var("ref_three", "${env:ref_four}");
    }

    init! {
        name : "custom",
        test : false,
        verbosity : "trace",
    }

    let _span = info_span!("my_span").entered();
    info!("Test done!");

    Ok(())
}

/// Test init! by providing a model::TracingConfig
#[test]
fn test_init_std_err() -> Result<()> {
    use common_macros::hash_map;

    let config = model::TracingConfig {
        title: "test init by config".to_owned(),
        writers: hash_map!(
            "stdout".into() => model::Writer::StandardOutput,
            "stderr".into() => model::Writer::StandardError
        ),
        layers: hash_map!(
            "fmt_layer".into() => model::Layer::Fmt(model::FmtLayer {
                filter: None,
                writer: "stderr".into(),
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
            "root".into() => model::Filter {
                level: model::Level::Trace,
                directives: None,
            }
        ),
    };

    init! {
        config : config,
        verbosity : "trace",
    }

    let _span = info_span!("my_span").entered();
    info!("This should be on stderr");

    Ok(())
}
