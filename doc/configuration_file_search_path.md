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