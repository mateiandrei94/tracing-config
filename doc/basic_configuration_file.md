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