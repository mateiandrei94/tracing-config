//! Primary error types for this crate.

use crate::interpolate::VarError;
use t::subscriber::SetGlobalDefaultError;

use thiserror::Error as ThisError;
use ts::filter::ParseError as TsFilterParseError;
use std::io::Error as StdIoError;
use toml::de::Error as TomlDeError;
use toml::ser::Error as TomlSerError;
use ta::rolling::InitError as TaRollingInitError;

/// The only error type, wraps other errors
///
/// There is no need for multiple error types since this crate is not meant to be used
/// extensively by applications, it is rather meant to be used trough a single function call in main()
/// with a `panic` if something goes wrong.
///
/// In future versions I might decide to split the errors so that, upon a function call, you only get
/// the specific error that specific function fails at, rather tan every possible error.
#[derive(ThisError, Debug)]
pub enum TracingConfigError {
    #[error("Tracing was already initialized")]
    AlreadyInitialized,
    #[error("Tracing config guard was dropped")]
    TracingConfigGuardDropped,
    #[error("Could not initialize because : `{0}`")]
    Other(String),
    #[error("The filter `{filter}` specified in layer `{layer}` was not found")]
    FilterNotFound { filter: String, layer: String },
    #[error("Could not parse the filter `{filter}` specified in layer `{layer}`")]
    FilterParseError {
        filter: String,
        layer: String,
        error: TsFilterParseError,
    },
    #[error("The writer `{writer}` specified in layer `{layer}` was not found")]
    WriterNotFound { writer: String, layer: String },
    #[error("The layer `{sifted_layer}` specified in layer `{layer}` was not found")]
    LayerNotFound { sifted_layer: String, layer: String },
    #[error("The layer `{sifted_layer}` specified in sifting layer `{layer}` has the following configuration error : `{error_message}`")]
    SiftingLayerConf {
        sifted_layer: String,
        layer: String,
        error_message: String,
    },

    #[error("Could not find environment variable : `{0}`")]
    MissingEnvironmentVariable(#[from] VarError),
    #[error("Configuration file does not exist or it could not be read : `{0}`")]
    IoError(#[from] StdIoError),
    #[error("Deserialization error, configuration file is either not syntactically a toml file or not a TracingConfig struct : `{0}`")]
    Deserialization(#[from] TomlDeError),
    #[error("Serialization error, configuration file could not be serialized : `{0}`")]
    Serialization(#[from] TomlSerError),
    #[error("Could not initialize a file writer : `{0}`")]
    FileInitError(#[from] TaRollingInitError),
    #[error("Could not set tracing global subscriber : `{0}`")]
    InitError(#[from] SetGlobalDefaultError),
    #[error("A lock was poisoned lock name = `{0}`")]
    PoisonError(String),
}
