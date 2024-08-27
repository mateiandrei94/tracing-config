# Tracing Config

**`NOTE`**: For `README.md` viewers, the links in the documentation do not work for you.
The documentation is best read online at 
[`docs.rs`](https://docs.rs/tracing-config/latest/tracing_config/) or locally.
To generate the documentation locally :
1) Download the source
2) `cargo doc --open`

The primary purpose of this crate is to allow rust programs to configure the [`tracing`]
crate using the [`Registry`] [`Subscriber`] implementation from the [`tracing-subscriber`]
crate given a [`toml`] configuration file and a simple [`init!()`] macro call in `main()` or
in any `#[test]` function.

This crate is not meant to be used by library authors. If your project contains a `lib.rs` file,
remove [`tracing-config`] from your `Cargo.toml` project file.

# Performance penalties / Memory overhead
If you use this crate to build and set up your global [`tracing`] [`Subscriber`],
the implementation will be a [`tracing-subscriber`] [`Registry`] and all [`Layer`]s added
to said [`Registry`] will be dynamic dispatch `Box<dyn Layer>`.
Moreover [`tracing-config`]'s own [`SpanRecordLayer`] will be added to the [`Registry`]
right after the root [`EnvFilter`] which will essentially keep an in memory [`serde_json Value`] 
representation of all (non filtered out) [`Span`] [`Value`]s practically negating any and
all performance gained by [`tracing`]s [`visitor pattern`] which does not keep an in-memory
representation of a span data after it's been created/entered.
The [`SpanRecordLayer`] visits the values and leverages [`tracing-subscriber`]s
[`span Extensions`], to persist span data for the remaining of the span's lifetime.

If you suspect that your application suffers performance penalties due to how tracing is configured:
- Submit a bug report
- Try a stricter filter or entirely remove some high verbosity tracing events
  (see [`level_filters`])
- Consider emitting less events, you should not debug your application using tracing,
  use a debugger instead.
- Try building your subscriber manually in `main()` doing so removes the need for
  dynamic dispatch layers.
- Lastly you can remove `tracing-config` from your `Cargo.toml` project file and
  find a different way to configure tracing.

`Note` : Given that there are a myriad of programming languages that only use dynamic dispatch or
heavily rely on it for logging/tracing purposes.
I think that having the same in rust is no big deal especially because once your configuration
is mature enough you can easily construct your subscriber without dynamic dispatch
or the [`SpanRecordLayer`].

# Getting started
#### `Cargo.toml`
```toml
tracing-config = { version = "0.2" }
tracing = { version = "0.1", features = [
    "max_level_trace", # trace for debug
    "release_max_level_info" # info for release
]}
```
#### `main.rs`
```rust
use tracing::*;
fn main() {
    tracing_config::init!(); // panics; read the docs on why and when.
    let _main_info_span = info_span!("main").entered();
    info!("Hello World");
}
```
#### Environment variable `tracing_config`
Set the environment variable so that it points directly to the `tracing.toml` file.

#### Current directory
If setting up an environment variable is too much work, you can also place `tracing.toml` in the
current directory (usually near `Cargo.toml`), `cargo run` as well as your IDE will both work.

#### Rudimentary configuration file.
#### `tracing.toml`
```toml
title = "Pretty colored ts-fmt to stdout"

[layer.ts-fmt]
type = "fmt"
writer = "stdout"
formatter = "pretty"
span_events = "none"
ansi = true

[writer.stdout]
type = "standard_output"

[filter.root]
level = "trace"
```

# Configuration file search path
#### Quick setup
- Place `tracing.toml` in the current directory (near `Cargo.toml`) and `cargo run`.
- You can set the `tracing_config` environment variable and have it point directly to a `.toml`
  configuration file.
- You can set the `tracing_config_test` environment variable if you want a separate configuration
  file for tests.

Both environment variables can also point to a directory containing :
- `tracing-${name}.toml`; where `${name}` is replaced by the `package.name` in
  your `Cargo.toml`.
- `tracing.toml`.
- ... And only for tests (`#[test]`), files with the `-test` suffix are checked first
  (i.e.: `tracing-${name}-test.toml` and `tracing-test.toml`).

#### Init parameters
The [`init!()`] macro or the [`initialize`] function can be supplied with:
- `path` : A [`Path`] pointing directly to a `.toml` configuration file or to a directory.
- `env` : The key of an environment variable, pointing directly to a `.toml` configuration file
  or to a directory.
- `name` defaults to `package.name` in your `Cargo.toml`.
- `qualifier` defaults to an empty string (read [`ProjectDirs`]).
- `organization` defaults to an empty string (read [`ProjectDirs`]).

#### Directories
In order to understand the directories mentioned in the following search path, please
read the documentation of the [`directories`] crate. [`ProjectDirs`] is constructed by default
with empty strings except for the `application` parameter which is the same `name` that you supply
to the [`init!()`] macro (that defaults to `package.name` if not set).  
`qualifier` and/or `organization` are optional but can either or both be supplied to [`init!()`]
which will forward them to the [`ProjectDirs`] constructor.

#### The search path
The `search path` is an ordered list of either `environment variables` or `files` or `directories`.
The first element in the list has the highest priority. The search function will loop trough all
elements from highest priority to least priority returning the first existing `.toml` configuration
file.

1) `path` (supplied in source to the [`init!()`] macro or the [`initialize`] function).
2) `env` (supplied in source to the [`init!()`] macro or the [`initialize`] function).
3) `env_parent` if `env` is set but does not exist.
4) only for tests: `tracing_config_test` environment variable.
5) only for tests: `tracing_config_test_parent` if `tracing_config_test` is set but does not exist.
6) `tracing_config` environment variable.
7) `tracing_config_parent` if `tracing_config` is set but does not exist.
8) [`project_dirs_preference_dir`]
9) [`project_dirs_config_dir`]
10) [`project_dirs_config_local_dir`]
11) [`base_dirs_preference_dir`]
12) [`base_dirs_config_dir`]
13) [`base_dirs_config_local_dir`]
14) [`user_dirs_home_dir`]
15) [`base_dirs_home_dir`]
16) [`current_exe_dir`]
17) [`current_dir`]

Within each directory in the `search path` the first file that matches the following is accepted :
 - `tracing-${name}.toml`; where `${name}` is replaced by the `package.name` in
  your `Cargo.toml` or by the `name` override passed to [`init!()`] or [`initialize`].
- `tracing.toml`
- ... And only for tests (`#[test]`), files with the `-test` suffix are checked first
  (e.g.: `tracing-${name}-test.toml`)

The `search path` is processed as follows :
- Environment variable entries (`env`, `tracing_config` and `tracing_config_test`) specifying
  (or pointing to) a direct `.toml` configuration file or directory are accepted if the
  file/directory exists, otherwise demoted to the directory in which the (non existing) file
  or directory resides (i.e.: it's parent).
- Entries specifying (or pointing to) a directory that does **NOT** exist are ignored.
- Environment variable values containing tokens in the form of `${env:key}`, are [`resolve`]d
  by replacing the token with the value of the environment variable specified by `key` if it exists;
  this is done recursively up to a certain depth (>=25).
- If an environment variable points to an existing `.toml` file, said file is accepted
  regardless of it's name, though calling it `tracing.toml` is recommended.

#### Debugging the initialization process
By default, during initialization, [`tracing-config`] will only emit errors and warnings in ansi
color to the program's standard output, this can be changed by setting a different `verbosity` level
when calling [`init!()`] (e.g: `init! { verbosity : "trace", };`) or
[`initialize`]`(Some(verbosity))` otherwise, an environment variable `tracing_config_verbosity` can
be set; accepted values are : `trace`, `debug`, `info`, `warn`, `error`, `none`.
Setting this to `debug` or `trace` will cause the function responsible to evaluate the `search path`
to "print" information about where it's looking and which file is accepted.

#### Suppressing output
Call [`init!()`] with `verbosity : "none"` this overrides the `tracing_config_verbosity` environment
variable, otherwise make sure it's value is set to "none", this is not recommended though, as the
default verbosity is set to `warn` which only outputs warnings that should be resolved and hard
errors which eventually panic if initialized by macro.

# Configuration file
To fully understand the nomenclature of the configuration file, a thorough read of the documentation
on both [`tracing`] and [`tracing-subscriber`] crates is required; however, here is a brief summary:
- `writer` is used by a `layer` and is responsible to write the data incoming form a `layer` to a
  destination, which can be anything, e.g.(standard_output, file, network, database, etc...).
  A writer as a component in the system is not strictly necessary since a `layer` could do the
  writing itself.
- `layer` is something that receives structured events and spans (i.e.: all the information that
  [`event!`] and [`span!`] macro calls contain) and is responsible to either ignore such events
  and spans or format them and either directly write somewhere or send the formatted events and
  spans to a `writer`.
- `filter` is a special kind of `layer` with the sole purpose of filtering out events and spans.
  A filter is exclusive, in that it allows everything by default unless there is an exclusion rule.

The `"flow"` that events and spans usually go trough is : `filter`->`layer`->`writer`.

You can find a detailed example and how the configuration file works
in the docs for the [`config::model module`].  
For a full understanding of the configuration file structure, start by reading the docs for the root
level configuration structure i.e.: a [`TracingConfig`] structure.

# Basic configuration file mini guide.
- `Note`: The configuration file can include environment variables in the form of `${env:key}`
  tokens in any toml string in the file, they are [`resolve`]d with `depth=25`.

In this basic example, we will configure a compact fmt layer with colors for the terminal/console.  
Next we will require an environment variable named `tracing_config_logs` (the name is arbitrary)
which will point to a directory where we will save both .log files and .json files in subdirectories
(.log for humans, and .json for machines).

- Start by setting the required `title`
```toml
title = "Basic configuration file"
```
- Declare the `layer` that will output in color to the terminal/console.  
- Call it `color-terminal` (again, name is arbitrary).
- The `type` of the layer is `fmt` from [`tracing-subscriber`]
- We will have to later declare a `writer` but we can name it now "terminal" (name is arbitrary).
- We want the `fmt` layer to use the `compact` `formatter`.
- Since this is the terminal, we disable `span_events`
- Lastly we enable `ansi` which means pretty colors.
```toml
[layer.color-terminal]
type = "fmt"
writer = "terminal"
formatter = "compact"
span_events = "none"
ansi = true
```
- We now have to declare the writer named `terminal` since the `layer` `color-terminal` is using it.
- We wanted the console as output so set the `type` to `standard_output` no other configuration
  is necessary for this type of writer.
```toml
[writer.terminal]
type = "standard_output"
```
- Next we need to declare 2 more layers, 1 that will write .log and the other that will write .json
- Let's start with the .log, it's the same as the `color-terminal` except :
- It's called `log-file`
- Set the formatter to `full`
- We want `active` `span_events` since these will emit an event on span enter and exit.
- This is a file, so we don't want `ansi`
- Lastly we will have to declare another writer, in this example I will give it the same name as
  the layer i.e.: log-file.
```toml
[layer.log-file]
type = "fmt"
writer = "log-file"
formatter = "full"
span_events = "active"
ansi = false
```
- Let's also declare the layer for the json file before starting with the `writer`s.
- We'll use [`tracing-config`]s custom json layer, set `type` to `json`.
- The default config for the json layer is acceptable, although the `pretty` flag is off by default,
  we set it explicitly here to show that pretty output is possible.
- We'll call the writer `machine-readable`.
```toml
[layer.json-file]
type = "json"
writer = "machine-readable"
pretty = false
```
- Now we have to declare 2 writers, one `log-file` and another `machine-readable`.
- Start with the `log-file`
- Set the `type` to `file`
- The output directory will be a `human` subdirectory inside whatever directory 
  the environment variable `tracing_config_logs` points to.
  We set `directory_path` with a reference to `${env:tracing_config_logs}`.
- Set `file_name` and `file_ext`
```toml
[writer.log-file]
type = "file"
directory_path = "${env:tracing_config_logs}/human"
file_name = "my_app"
file_ext = "log"
max_log_files = 7 # keep at most 7 log files
rotation = "daily" # 7 log files daily means a week of history
non_blocking = true # async writes
lossy = true # it's okay if we loose some, we still have the json
```
- The json file now
```toml
[writer.machine-readable]
type = "file"
directory_path = "${env:tracing_config_logs}/machine"
file_name = "my_app"
file_ext = "json"
rotation = "never" # This will be GB in size
non_blocking = true
lossy = false
```
- Lastly the root filter.
```toml
[filter.root]
level = "trace"
directives = [
    "hyper=error",
    "hyper::client::connect::dns=error",
    "hyper::proto::h1::conn=error",
    "hyper::proto::h1::conn=error",
    "hyper::proto::h1::io=error",
    "hyper::proto::h1::role=error",
    "hyper::proto::h1::encode=error",
    "hyper::client::pool=error",
]
```

Combining it together:

#### `tracing-basic.toml`
```toml
title = "Basic configuration file"

[layer.color-terminal]
type = "fmt"
writer = "terminal"
formatter = "compact"
span_events = "none"
ansi = true

[writer.terminal]
type = "standard_output"

[layer.log-file]
type = "fmt"
writer = "log-file"
formatter = "full"
span_events = "active"
ansi = false

[layer.json-file]
type = "json"
writer = "machine-readable"
pretty = false

[writer.log-file]
type = "file"
directory_path = "${env:tracing_config_logs}/human"
file_name = "my_app"
file_ext = "log"
max_log_files = 7 # keep at most 7 log files
rotation = "daily" # 7 log files daily means a week of history
non_blocking = true # async writes
lossy = true # it's okay if we loose some, we still have the json

[writer.machine-readable]
type = "file"
directory_path = "${env:tracing_config_logs}/machine"
file_name = "my_app"
file_ext = "json"
rotation = "never" # This will be GB in size
non_blocking = true
lossy = false

[filter.root]
level = "trace"
directives = [
    "hyper=error",
    "hyper::client::connect::dns=error",
    "hyper::proto::h1::conn=error",
    "hyper::proto::h1::conn=error",
    "hyper::proto::h1::io=error",
    "hyper::proto::h1::role=error",
    "hyper::proto::h1::encode=error",
    "hyper::client::pool=error",
]
```
- Test it
```rust
#[test]
fn test_basic_config() {
  use tracing::*;
  std::env::set_var("tracing_config_logs", "path/to/some/dir");

  tracing_config::init! {
    path : std::path::Path::new("path/to/tracing-basic.toml")
  };

  // do more tests here
  let _span = info_span!("my_span").entered();
  info!("Test done!");
}
```

[`tracing`]: mod@::tracing
[`Subscriber`]: trait@::tracing::Subscriber
[`Span`]: struct@::tracing::Span
[`Value`]: trait@::tracing::Value
[`visitor pattern`]: trait@::tracing::field::Visit
[`level_filters`]: mod@::tracing::level_filters
[`event!`]: macro@::tracing::event
[`span!`]: macro@::tracing::span
[`set_global_default`]: fn@::tracing::subscriber::set_global_default

[`tracing-subscriber`]: mod@::tracing_subscriber
[`Layer`]: trait@::tracing_subscriber::Layer
[`Registry`]: struct@::tracing_subscriber::registry::Registry
[`EnvFilter`]: struct@::tracing_subscriber::filter::EnvFilter
[`span Extensions`]: struct@::tracing_subscriber::registry::Extensions

[`tracing-config`]: mod@crate
[`init!()`]: macro@crate::init
[`initialize`]: fn@crate::config::initialize
[`SpanRecordLayer`]: struct@crate::tracing::SpanRecordLayer
[`resolve`]: fn@crate::interpolate::resolve_from_env_recursive
[`config::model module`]: mod@crate::config::model
[`TracingConfig`]: struct@crate::config::model::TracingConfig
[`TracingConfigError`]: enum@crate::TracingConfigError
[`TracingConfigError::AlreadyInitialized`]: type@crate::TracingConfigError::AlreadyInitialized
[`search path`]: mod@crate#configuration-file-search-path

[`tracing-appender`]: mod@::tracing_appender

[`serde`]: mod@::serde
[`serde_json`]: mod@::serde_json
[`serde_json Value`]: enum@::serde_json::value::Value

[`toml`]: mod@::toml
[`uuid`]: mod@::uuid

[`directories`]: mod@::directories
[`ProjectDirs`]: struct@::directories::ProjectDirs
[`project_dirs_preference_dir`]: fn@::directories::ProjectDirs::preference_dir
[`project_dirs_config_dir`]: fn@::directories::ProjectDirs::config_dir
[`project_dirs_config_local_dir`]: fn@::directories::ProjectDirs::config_local_dir
[`base_dirs_preference_dir`]: fn@::directories::BaseDirs::preference_dir
[`base_dirs_config_dir`]: fn@::directories::BaseDirs::config_dir
[`base_dirs_config_local_dir`]: fn@::directories::BaseDirs::config_local_dir
[`user_dirs_home_dir`]: fn@::directories::UserDirs::home_dir
[`base_dirs_home_dir`]: fn@::directories::BaseDirs::home_dir
[`current_exe_dir`]: fn@::std::env::current_exe
[`current_dir`]: fn@::std::env::current_dir

[`Path`]: struct@::std::path::Path