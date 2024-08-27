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