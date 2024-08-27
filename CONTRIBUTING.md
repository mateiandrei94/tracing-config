# Contributing to Tracing Config

Contributions are welcome, but please don’t start programming without discussing your ideas first,
because your work may not be accepted in the project.

If you would like to contribute, follow these steps:

1. Create an issue.
2. Clearly describe whether it’s a bug, improvement, feature request, etc.
3. Indicate if you're willing to work on the issue.
4. Wait for my feedback before starting any work.

Let's collaborate to ensure your effort is aligned with the project's goals and not wasted.

If you are displeased with me, the project is licensed under MIT, you can always create a fork...
But whatever it is, I'm sure we can talk it out and reach an agreement.

# Guide

**Synopsis**

* `cargo build`
* `cargo build --release`
* `cargo doc --document-private-items`
* `cargo watch --clear --quiet --exec "doc --document-private-items"`
* `cargo fmt --check`
* `cargo fmt`

**Compiling**

* Using the latest version of `rustc` and `cargo`
* `cargo build`, for release : `cargo build --release`

**Documentation**

Public api must be documented, private is preferred.

* Using the latest version of `rust` and `cargo`
* `cargo doc --document-private-items`

Calling `cargo doc` multiple times gets old fast, you can use `cargo-watch` which
does that automatically on save:

* Install using `cargo install cargo-watch`
* The command is : `cargo watch --clear --quiet --exec "doc --document-private-items"`

Whenever repeating, the documentation should be written in the `doc` folder, then included.
The main `README.md` is generated at build time by `build.rs` so don't modify it manually.

**Source code Formatting**

The `cargo fmt` command will re format source code changing it, this sometimes messes with git.
To avoid this :

1. Before applying any change, make sure `cargo fmt --check` does not output anything.
2. If it does output something, run `cargo fmt` and create a commit with message `formatted source`,
    the change should only be whitespace.
3. Apply your changes with or without formatting
4. Run `cargo fmt`
5. Commit

**Committing**

When developing I generally make small meaningless commits locally, squash everything before a push.
I dislike seeing a roadmap of every single word that changed in a file.
Whether full or partial a change should be 1 commit in this project.
A review is then carefully done on all the changes.
This is to prevent `frog boiling` where small commits amount to nothing, but together they become
malware (like what happened to other projects).