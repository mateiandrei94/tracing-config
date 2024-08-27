use std::fmt::Debug;
use std::collections::HashMap;
use std::hash::Hash;
use std::sync::RwLock;

use serde_json::value::Value as JsonValue;

use t::{Subscriber, Event as TEvent, Level as TLevel};
use t::span::{Id as SpanId, Record as TSpanRecord, Attributes as TSpanAttributes};

use ts::layer::{Context, Layer as TsLayer};
use ts::registry::{SpanRef, LookupSpan};

use super::SpanRecord;

/// A Selector is a set of span metadata and span keys, it's purpose is to sift spans.
#[derive(Debug, Clone, Default, Hash, PartialEq, Eq)]
pub struct Selector {
    level: bool,
    name: bool,
    target: bool,
    module_path: bool,
    file: bool,
    line: bool,
    fields: Vec<String>,
}

/// Given a [`Selector`][struct@Selector], `Values` contains the values of a certain span.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Values {
    pub level: Option<TLevel>,
    pub name: Option<&'static str>,
    pub target: Option<String>,
    pub module_path: Option<String>,
    pub file: Option<String>,
    pub line: Option<u32>,
    pub field_values: Vec<Option<String>>,
}

/// A [`tracing-subscriber`][mod@ts] [`Layer`][trait@TsLayer] that sifts trough spans and forwards events to dynamically created Layers.
///
/// Just like all the layers it's generic over `S` which is a `Subscriber`. It is also generic over `L` which is the dynamically created layer and `FnLB` (LB stand for Layer Builder) which is the closure responsible to create a new Layer at runtime.
///
/// The `FnLB` will receive a [`Selector`][struct@Selector] and a [`SelectorValues`][struct@Values] and is responsible to create a unique layer for the specific set of `SelectorValues`.
/// `FnLB` will be called once for every distinct set of `SelectorValues`.
pub struct Layer<S, L, FnLB>
where
    S: Subscriber,
    S: for<'lookup> LookupSpan<'lookup>,
    L: TsLayer<S> + Send + Sync + 'static,
    FnLB: FnMut(&Selector, &Values) -> L,
{
    sift_selector: Selector,
    layer_cache: RwLock<HashMap<Values, L>>,
    layer_builder: RwLock<FnLB>,
    _marker: std::marker::PhantomData<S>,
}

impl Selector {
    pub fn new() -> Self {
        Self {
            level: false,
            name: false,
            target: false,
            module_path: false,
            file: false,
            line: false,
            fields: Vec::new(),
        }
    }

    pub fn level(mut self) -> Self {
        self.level = true;
        self
    }

    pub fn name(mut self) -> Self {
        self.name = true;
        self
    }

    pub fn target(mut self) -> Self {
        self.target = true;
        self
    }

    pub fn module_path(mut self) -> Self {
        self.module_path = true;
        self
    }

    pub fn file(mut self) -> Self {
        self.file = true;
        self
    }

    pub fn line(mut self) -> Self {
        self.line = true;
        self
    }

    pub fn field(mut self, field: &str) -> Self {
        self.fields.push(field.to_owned());
        self
    }

    pub fn index_of(&self, key: &str) -> Option<usize> {
        for (i, k) in self.fields.iter().enumerate() {
            if key == k {
                return Some(i);
            }
        }
        None
    }

    /// Given an `input` string and [`SelectorValues`][struct@Values] will replace placeholders.
    pub fn resolve_variable(&self, input: &str, values: &Values) -> String {
        // static REGEX_STR: &str = r"\$\{sl\:([^}]+)\}";

        use crate::interpolate::*;

        resolve_infallible(input, |scheme, key| {
            if scheme != "sl" {
                return None;
            }

            let s = match key {
                "meta:level" => values.level.map(|level| level.to_string()),
                "meta:name" => values.name.map(|name| name.to_owned()),
                "meta:target" => values.target.clone(),
                "meta:module_path" => values.module_path.clone(),
                "meta:file" => values.file.clone(),
                "meta:line" => values.line.map(|line| format!("{line}")),
                key => {
                    if let Some(idx) = self.index_of(key) {
                        if let Some(Some(val)) = values.field_values.get(idx) {
                            Some(val.to_owned())
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
            };

            if let Some(s) = s {
                Some(s)
            } else {
                Some("none".to_owned())
            }
        })
    }
}

impl Values {
    pub fn new<S>(sift_selector: &Selector, span: Option<SpanRef<'_, S>>) -> Self
    where
        S: Subscriber,
        S: for<'lookup> LookupSpan<'lookup>,
    {
        let span = match span {
            Some(span) => span,
            None => {
                return Self {
                    level: None,
                    name: None,
                    target: None,
                    module_path: None,
                    file: None,
                    line: None,
                    field_values: vec![None; sift_selector.fields.len()],
                };
            }
        };

        let metadata = span.metadata();

        let module_path = if sift_selector.module_path {
            metadata
                .module_path()
                .map(|module_path| module_path.to_owned())
        } else {
            None
        };

        let file = if sift_selector.file {
            metadata.file().map(|file| file.to_owned())
        } else {
            None
        };

        let level = if sift_selector.level {
            Some(*metadata.level())
        } else {
            None
        };
        let name = if sift_selector.name {
            Some(metadata.name())
        } else {
            None
        };
        let target = if sift_selector.target {
            Some(metadata.target().to_owned())
        } else {
            None
        };
        let line = if sift_selector.line {
            metadata.line()
        } else {
            None
        };

        let mut field_values: Vec<Option<String>> = vec![None; sift_selector.fields.len()];
        get_field_values_recursive(&sift_selector.fields, &mut field_values, &span);

        return Self {
            level,
            name,
            target,
            module_path,
            file,
            line,
            field_values,
        };

        // private inner helper function
        fn get_field_values_recursive<S>(
            fields_keys: &Vec<String>,
            fields_values: &mut Vec<Option<String>>,
            span: &SpanRef<'_, S>,
        ) where
            S: Subscriber,
            S: for<'lookup> LookupSpan<'lookup>,
        {
            let span_extensions = span.extensions();
            if let Some(span_values) = span_extensions.get::<SpanRecord>() {
                for (key_index, key) in fields_keys.iter().enumerate() {
                    match fields_values.get(key_index) {
                        // the None case never happens
                        // while Some(Some(_)) means that a value already exists and there is no need to override it.
                        None | Some(Some(_)) => continue,
                        // space for the value exists but it's not yet set.
                        Some(None) => {
                            if let Some(json_value) = span_values.map.get(key as &str) {
                                let value = json_value_to_string(json_value);
                                fields_values.insert(key_index, Some(value));
                            }
                        }
                    }
                }
                if let Some(span_parent) = span.parent() {
                    get_field_values_recursive(fields_keys, fields_values, &span_parent);
                }
            }
        }
    }
}

impl<S, L, FnLB> Layer<S, L, FnLB>
where
    S: Subscriber,
    S: for<'lookup> LookupSpan<'lookup>,
    L: TsLayer<S> + Send + Sync + 'static,
    FnLB: FnMut(&Selector, &Values) -> L,
{
    /// To create a `SiftingLayer` a selector must be provided as well as a closure that creates layers based on the selectors values.
    pub fn new(sift_selector: Selector, layer_builder: FnLB) -> Self {
        Self {
            sift_selector,
            layer_cache: RwLock::new(HashMap::new()),
            layer_builder: RwLock::new(layer_builder),
            _marker: std::marker::PhantomData {},
        }
    }

    fn get_layer<F>(&self, ssv: Values, f: F)
    where
        F: FnOnce(&L),
    {
        {
            let layer_cache = rw_lock_read(&self.layer_cache, "layer_cache");

            if let Some(layer) = layer_cache.get(&ssv) {
                f(layer);
                return;
            }
        }

        let mut layer_cache = rw_lock_write(&self.layer_cache, "layer_cache");

        if let Some(layer) = layer_cache.get(&ssv) {
            f(layer);
            return;
        }

        let layer = self.build_layer(&ssv);

        f(&layer);

        layer_cache.insert(ssv, layer);
    }

    fn build_layer(&self, ssv: &Values) -> L {
        let mut layer_builder = rw_lock_write(&self.layer_builder, "layer_builder");
        layer_builder(&self.sift_selector, ssv)
    }
}

impl<S, L, FnLB> TsLayer<S> for Layer<S, L, FnLB>
where
    S: Subscriber,
    S: for<'lookup> LookupSpan<'lookup>,
    L: TsLayer<S> + Send + Sync + 'static,
    FnLB: FnMut(&Selector, &Values) -> L + 'static,
{
    fn on_new_span(&self, attrs: &TSpanAttributes<'_>, id: &SpanId, ctx: Context<'_, S>) {
        let ssv = Values::new(&self.sift_selector, ctx.span(id));
        self.get_layer(ssv, |layer| {
            layer.on_new_span(attrs, id, ctx);
        });
    }

    fn on_record(&self, id: &SpanId, values: &TSpanRecord<'_>, ctx: Context<'_, S>) {
        let ssv = Values::new(&self.sift_selector, ctx.span(id));
        self.get_layer(ssv, |layer| {
            layer.on_record(id, values, ctx);
        });
    }

    fn on_enter(&self, id: &SpanId, ctx: Context<'_, S>) {
        let ssv = Values::new(&self.sift_selector, ctx.span(id));
        self.get_layer(ssv, |layer| {
            layer.on_enter(id, ctx);
        });
    }

    fn on_exit(&self, id: &SpanId, ctx: Context<'_, S>) {
        let ssv = Values::new(&self.sift_selector, ctx.span(id));
        self.get_layer(ssv, |layer| {
            layer.on_exit(id, ctx);
        });
    }

    fn on_close(&self, id: SpanId, ctx: Context<'_, S>) {
        let ssv = Values::new(&self.sift_selector, ctx.span(&id));
        self.get_layer(ssv, |layer| {
            layer.on_close(id, ctx);
        });
    }

    fn on_event(&self, event: &TEvent<'_>, ctx: Context<'_, S>) {
        let span = ctx.current_span();
        let span = if let Some(id) = span.id() {
            ctx.span(id)
        } else {
            None
        };
        let ssv = Values::new(&self.sift_selector, span);
        self.get_layer(ssv, |layer| {
            layer.on_event(event, ctx);
        });
    }
}

#[inline(always)]
fn rw_lock_read<'a, T>(
    rw_lock: &'a RwLock<T>,
    lock_name: &str,
) -> std::sync::RwLockReadGuard<'a, T> {
    match rw_lock.read() {
        Ok(v) => v,
        Err(v_error) => {
            print_error(lock_name, "read", "lock poisoned");
            v_error.into_inner()
        }
    }
}

#[inline(always)]
fn rw_lock_write<'a, T>(
    rw_lock: &'a RwLock<T>,
    lock_name: &str,
) -> std::sync::RwLockWriteGuard<'a, T> {
    match rw_lock.write() {
        Ok(v) => v,
        Err(v_error) => {
            print_error(lock_name, "read", "lock poisoned");
            v_error.into_inner()
        }
    }
}

#[inline(always)]
fn print_error(name: &str, func: &str, msg: &str) {
    let now = chrono::Local::now();
    let error = format!("{now} -> ERROR : SiftingLayer -- {name}.{func}() {msg} !");
    println!("{error}");
    eprintln!("{error}");
}

#[inline(always)]
fn json_value_to_string(value: &JsonValue) -> String {
    match value {
        JsonValue::String(s) => s.clone(),
        _ => value.to_string(),
    }
}

#[test]
fn to_some_repl_test() {
    let s = Selector::new().field("k").field("ve");

    let v = Values {
        level: Some(TLevel::INFO),
        name: None,
        target: None,
        module_path: None,
        file: None,
        line: None,
        field_values: {
            let mut v = Vec::new();
            v.push(Some("k_v".to_owned()));
            v.push(None);
            v
        },
    };

    let k = "some good string `${sl:meta:level}` containing : `${sl:k}` ${sl:ve} and `${sl:k}` ${sl:k2} ${sl:k} then some".to_string();
    let r = s.resolve_variable(&k, &v);

    println!("v = {r}");
}
