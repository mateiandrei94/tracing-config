// #![deny(missing_docs)]
#![forbid(unsafe_code)]

//! The primary purpose of this crate is to allow rust programs to configure the [`tracing`][mod@t]
//! and [`tracing-subscriber`][mod@ts] crates using a `toml` configuration file.
//!
//! This crate is not meant to be used by library authors.
//! If your project contains a `lib.rs` file. Please remove `tracing-config` from your `Cargo.toml` project file.
//!
//! # Performance penalties / Memory overhead
//! If you use this crate to build and set up your global `tracing` [`Subscriber`][trait@t::Subscriber],
//! the implementation will be a `tracing-subscriber` [`Registry`][struct@ts::registry::Registry] and
//! all [`Layer`][trait@ts::Layer]s added to said `Registry` will be dynamic dispatch `Box<dyn Layer>`.
//! Moreover the `tracing-config`'s own [`SpanRecordLayer`][struct@tracing::SpanRecordLayer] will be added to the `Registry`
//! right after the root [`EnvFilter`][struct@ts::filter::EnvFilter]
//! which will essentially keep an in memory [`json`][enum@serde_json::value::Value] representation of all (non filtered out) [`Span`][t::Span] [`data/field`][mod@t::field]s
//! practically negating any and all performance gained by the `tracing` "visitor pattern" (see [`Visit`][trait@t::field::Visit]) which does not keep an in-memory representation for any event;
//! This leverages the `tracing-subscriber` Span [`Extensions`][struct@ts::registry::Extensions].
//!
//! If you suspect that your application suffers performance penalties due to how tracing is configured:
//! - Submit a bug report
//! - Try a stricter filter or entirely remove some high verbosity tracing events (see [`level_filters`][mod@t::level_filters])
//! - Consider emitting less events, you should not debug your application using tracing, use a debugger instead.
//! - Try building your subscriber manually in main() doing so removes the need for dynamic dispatch layers.
//! - Lastly you can remove `tracing-config` from your `Cargo.toml` project file and find a different way to configure tracing.
//!
//! Given that there are a myriad of programming languages that only use dynamic dispatch or heavily rely on it for logging/tracing purposes.
//! I think that having the same in rust is no big deal especially because once your configuration is mature enough
//! you can easily construct your subscriber without dynamic dispatch.
//!
//! # Getting started
//! ```toml
//! # Cargo.toml
//! tracing-config = { version = "0.1" }
//! tracing = { version = "0.1", features = [
//!     "max_level_trace", # compile time static, removes tracing macro calls at compile time for debug builds
//!     "release_max_level_trace" # compile time static, removes tracing macro calls at compile time for release builds
//! ]}
//! tracing-subscriber = { version = "0.3", features = [
//!     "chrono", # timestamps
//!     "registry", # shared span storage and base subscriber
//!     "env-filter", # filter events/spans at runtime
//!     "fmt", # event/span formatter
//!     "ansi", # for color terminal output
//!     "json", # json
//! ]}
//! tracing-appender = { version = "0.2" } # allows events and spans to be recorded in a non-blocking manner
//! ```
//! ```rust
//! // main.rs
//! use tracing::info;
//! use tracing::info_span;
//! fn main() {
//!     // Beware, the init() function panics ! Read the docs.
//!     // Careful not to drop the guard early, this results in a panic in certain cases.
//!     let _tc_guard = tracing_config::init();
//!     info_span!("main");
//!     info!("Hello World");
//! }
//! ```
//!
//! # For tests
//!```rust no_run
//! // some_module.rs
//! use tracing::info;
//! use tracing::info_span;
//! #[test]
//! fn test_some_functionality() {
//!     // Beware, the init_test() function panics ! Read the docs.
//!     // Careful not to drop the guard early, this results in a panic in certain cases.
//!     let _tc_guard = tracing_config::init_test();
//!     info_span!("functionality test");
//!     info!("Hello World");
//! }
//! ```
//!
//! # Configuration file search path
//! The [`init`][fn@init] function will load a `tracing.toml` configuration file given a default search path.
//! To understand the `search path` please read the documentation at [`find_config_path`][fn@config::find_config_path]
//! and/or enable `debug_mode` by setting the environment variable `tracing_config_debug`=`true` and monitoring your program standard output.
//!
//! The easiest/fastest way to set a specific configuration file is to have the `tracing_config` environment variable point directly to a specific `tracing.toml` file.
//! Otherwise it could point to a directory containing said file. (again, please read the documentation at [`find_config_path`][fn@config::find_config_path]).
//!
//! # Configuration file
//! To fully understand the nomenclature of the configuration file, a thorough read of the documentation on both [`tracing`][mod@t]
//! and [`tracing-subscriber`][mod@ts] crates is required; however, in short words:
//! - a `writer` is something that receives formatted events (i.e.: some sort of string + some metadata) and is responsible to write this data to a file or trough other means such as sending a network packet to some server.
//! - a `layer` is something that receives structured events and is responsible to format such events in a way that a writer can understand.
//! - a `filter` is something that receives structured events and is responsible to either accept or reject them.
//!
//! The `"flow"` is usually `filter`->`layer`->`writer`.
//!
//! You can find a detailed example of a configuration file at module level docs for [`config`][mod@config::model],
//! for a full understanding of the configuration file structure, start by reading the docs for the root level structure i.e.: a [`TracingConfig`][struct@config::model::TracingConfig] structure.
//!
//! # Rudimentary configuration file.
//! - `Note`: The configuration file can include environment variables in the form of `${env:key}` tokens where a toml string is present, for more details, read the [`config`][mod@config] module level docs and the [`interpolate`][mod@interpolate] module level docs.
//! ```toml
//! # tracing.toml
//! title = "Pretty colored ts-fmt to stdout"
//!
//! [layer.ts-fmt]
//! type = "fmt"
//! writer = "stdout"
//! formatter = "pretty"
//! span_events = "none"
//! ansi = true
//!
//! [writer.stdout]
//! type = "standard_output"
//!
//! [filter.root]
//! level = "trace"
//! ```
//!
//! # Public modules
//! - [`config`][mod@config]: Reads a configuration file, creates a tracing subscriber from it and initializes tracing's global subscriber.
//! - [`tracing`][mod@tracing]: Additional tracing-subscriber layers and or other tracing expansions.
//! - [`interpolate`][mod@interpolate]: Resolve (recursively) `${scheme:key}` tokens in a given `input` string.

// public crate level modules.

pub mod config;
pub mod interpolate;
pub mod tracing;

// private crate level modules.

mod error;

// re-export the basic public api;
// advanced usage should require specific imports.

pub use self::config::init;
pub use self::config::try_init;
pub use self::config::init_test;
pub use self::config::init_path;
pub use self::config::TracingConfigGuard;
pub use self::error::TracingConfigError;