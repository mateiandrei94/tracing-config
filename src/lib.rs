#![forbid(unsafe_code)]
// #![warn(missing_docs)]
// #![warn(rustdoc::private_intra_doc_links)]
#![cfg_attr(docsrs, feature(doc_cfg))]
// I include prism.js because rust doc does not have syntax highlighting for toml code sections.
#![doc = include_str!("../README.md")]
#![doc = include_str!("../doc/prism_js.html")]

pub mod tracing;
pub mod config;
pub mod interpolate;

#[macro_use]
mod macros; // Contains init!() macro.
mod error;

pub use self::config::initialize;
pub use self::config::TracingConfigGuard;
pub use self::error::TracingConfigError;

#[cfg(feature = "macros")]
pub use tracing_config_macros::test;