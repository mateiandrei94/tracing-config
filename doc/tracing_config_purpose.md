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