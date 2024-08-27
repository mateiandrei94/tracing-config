Initializes [`tracing`] by calling [`set_global_default`].

The `config` parameter is used to construct a [`Subscriber`] otherwise the [`search path`] is
employed to find a toml configuration file to construct a [`TracingConfig`] object.

#### Differences between [`init!()`] and [`initialize`]:

|                          |[`initialize`]            |[`init!()`]|
|--------------------------|--------------------------|-----------|
|**Panics**                |No                        |Yes, except for `AlreadyInitialized`, this is on purpose, we don't want to start our service and find out later that we don't have logs.|
|**Default parameters**    |No                        |Yes, see table below|
|**Ensures guard is used** |Compiler warning only     |Yes        |
|**Call complexity**       |Lots of parameters to pass|Yes, only specify what is needed|

The initialization is guarded by a mutual exclusion lock, so it can only happen once.
Either [`init!()`] or [`initialize`] must only be called once.

If `config` is supplied to the [`init!()`] macro or to the [`initialize`] function
the [`search path`] is disabled and the provided `config` object will be used to
initialize [`tracing-config`]. In such case all other parameters are `ignored`.

#### Default parameters

- The [`init!()`] macro calls [`initialize`] with default parameters where `not set` equates to
  `Option::None`.

|Parameter   |Type                |Default                                                                           |Description                  |
|------------|--------------------|----------------------------------------------------------------------------------|-----------------------------|
|qualifier   |`&str`              |`""` (empty string)                                                               |used by the [`search path`] for [`ProjectDirs`]|
|organization|`&str`              |`""` (empty string)                                                               |used by the [`search path`] for [`ProjectDirs`]|
|name        |`&str`              |`env!("CARGO_PKG_NAME")`                                                          |used by the [`search path`], the default is the package name in `Cargo.toml`|
|test        |`bool`              |`cfg!(test)`                                                                      |If `true` the [`search path`] will prioritize `.toml` files that have a `-test` suffix.|
|verbosity   |`&str`              |Value of environment variable `tracing_config_verbosity` if set, otherwise `warn`.|Can be one of: `trace`, `debug`, `info`, `warn`, `error`, `none`. Anything else and it defaults to `warn`.|
|env         |`&str`              |not set                                                                           |Add a priority environment variable key to the [`search path`].|
|path        |`&`[`Path`]         |not set                                                                           |Add a priority path to the [`search path`].|
|config      |[`TracingConfig`]   |not set                                                                           |Load configuration from source rather than file, if set: all other parameters are **ignored**.|

The syntax of the [`init!()`] macro is the same as `struct initialization` except that
all parameters are optional, you only specify them if you wish to override the default value.

The [`init!()`] macro does **NOT** return anything, it was purposely designed to be non hygienic
because it will declare a non accessible local variable containing a guard that must NOT be dropped.
On drop the guard will flush all remaining events to async workers and close everything.
A premature drop will most likely cause the program to panic.

The [`init!()`] macro will panic on any [`TracingConfigError`] other than
[`TracingConfigError::AlreadyInitialized`].

#### A few call examples
```rust
fn main() -> {
    use tracing_config::init;
    // obviously don't call init multiple times
    init!(); // panics if cannot initialize
    init! {
        organization: "CoolSoftware",
        qualifier: "com.example",
        name: "MyApp",
    };
    init! {
        verbosity: "trace",
    };
    init!{
        path: std::path::Path::new("/path/to/tracing.toml"),
    };
}
```