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