//! Primary error types for this crate.

use crate::interpolate::VarError;

use thiserror::Error as ThisError;
use ::tracing_subscriber::filter::ParseError as TsFilterParseError;
use std::io::Error as StdIoError;
use toml::de::Error as TomlDeError;
use toml::ser::Error as TomlSerError;
use ::tracing_appender::rolling::InitError as TaRollingInitError;

/// The only error type in this crate, wraps other errors.
///
/// There is no need for multiple error types since this crate is not meant to be used
/// extensively by applications, it is rather meant to be used through a single init macro call in main()
/// with a `panic` if something goes wrong.
///
/// In future versions I might decide to split the errors so that, upon any function call, you only get
/// the specific error that specific function fails at, rather than every possible crate error.
#[derive(ThisError, Debug)]
pub enum TracingConfigError {
    /// Invalid Level
    #[error("Invalid Level : {level}")]
    InvalidLevel {
        /// The invalid level name
        level: String,
    },
    /// Could not find configuration file
    #[error("Could not find configuration file")]
    ConfigFileNotFound,
    /// TracingConfig was already initialized
    #[error("Tracing was already initialized")]
    AlreadyInitialized,
    /// TracingConfig guard was dropped, do not drop the guard that the initialize function returns
    #[error("Tracing config guard was dropped")]
    TracingConfigGuardDropped,
    /// A filter was referenced or is required, but it is not declared in the tracing.toml file
    #[error("The filter `{filter}` specified in layer `{layer}` was not found")]
    FilterNotFound {
        /// The name of the filter
        filter: String,
        /// The layer referencing the filter
        layer: String,
    },
    /// Could not parse a filter / directive
    #[error("Could not parse the filter `{filter}` specified in layer `{layer}`")]
    FilterParseError {
        /// The name of the filter
        filter: String,
        /// The layer referencing the filter
        layer: String,
        /// Indicates that a string could not be parsed as a filtering directive.
        error: TsFilterParseError,
    },
    /// A writer was referenced but not declared
    #[error("The writer `{writer}` specified in layer `{layer}` was not found")]
    WriterNotFound {
        /// The name of the writer
        writer: String,
        /// The layer referencing the filter
        layer: String,
    },
    /// A referenced layer could not be found
    #[error("The layer `{sifted_layer}` specified in layer `{layer}` was not found")]
    LayerNotFound {
        /// The name of the layer that could not be found
        sifted_layer: String,
        /// The layer that is referencing the layer that could not be found
        layer: String,
    },
    /// There is a problem with how the Sifting Layer was configured
    #[error("The layer `{sifted_layer}` specified in sifting layer `{layer}` has the following configuration error : `{error_message}`")]
    SiftingLayerConf {
        /// The name of the referenced layer that is misconfigured
        sifted_layer: String,
        /// The name of the sifting layer
        layer: String,
        /// The error message
        error_message: String,
    },
    /// An environment variable was referenced but it's missing
    #[error("Could not find environment variable : `{0}`")]
    MissingEnvironmentVariable(#[from] VarError),
    /// An IoError occurred
    #[error("Configuration file does not exist or it could not be read : `{0}`")]
    IoError(#[from] StdIoError),
    /// Could not deserialize a toml file.
    #[error("Deserialization error, configuration file is either not syntactically a toml file or not a TracingConfig struct : `{0}`")]
    Deserialization(#[from] TomlDeError),
    /// Could not serialize a toml file.
    #[error("Serialization error, configuration file could not be serialized : `{0}`")]
    Serialization(#[from] TomlSerError),
    /// Could not initialize a file writer
    #[error("Could not initialize a file writer : `{0}`")]
    FileInitError(#[from] TaRollingInitError),
    /// An internal lock was poisoned, this shouldn't happen, if it does it's a bug in TracingConfig
    #[error("A lock was poisoned lock name = `{0}`")]
    PoisonError(String),
}
