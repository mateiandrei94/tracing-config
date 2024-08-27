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