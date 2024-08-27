//! Perform `${scheme:key}` placeholder replacement given an `input` string.
//!
//! **NOTE** : No function in this module support placeholder "escaping", this may change in future releases. As of this release :
//! - `scheme` is not allowed to contain `:` or `}` as `:` denotes a separation between scheme and key and `}` denotes the end of the placeholder.
//! - `key` is not allowed to contain `}` as it denotes the end of the placeholder.
//!
//! This module contains several `resolve` and `resolve_recursive` functions which revolve around the same concept i.e.: replace all occurrences of `${scheme:key}` in a given string. Replacement occurs by `key` in the given `scheme`.
//!
//! - The `scheme` indicates the method or protocol, you can imagine a `scheme` as a `HashMap`, a good example are environment variables for example `env` represents environment variables. Other example schemes could be a `json` object or a `toml` table. Schemes are arbitrary, they are only useful as long as a resolver function recognizes them.
//! - The `key` indicates a key to be retrieved from the selected method or protocol, for example the key `path` with the `env` method or protocol resolves to the system's path environment variable. The placeholder of which looks like `${env:path}`
//!
//! # Common `scheme`s
//! Although the `scheme` is arbitrary and all functions in this module either accept a `match_scheme` parameter or a `resolver` function which can freely choose it's recognized schemes;
//! other modules of this crate may use specific constant or "set in stone" schemes.
//! - `env` : represents environment variables
//! - `meta` : context specific metadata (usually a tracing span metadata)
//! - `sl` and `sl:meta` : specific schemes used by the `SiftingLayer`.
//!
//! `Note`: The default resolver functions implemented in this module return errors rather than silently ignore them by not replacing tokens with known recognized/common schemes.
//!
//! # Recursive versions
//! The `_recursive` version of a function will iteratively call it's non recursive counterpart with the same `cache` up to `depth` times by feeding in as `input` it's own output.
//!
//! The only added benefit of using a `_recursive` version of a function is therefore a per `scheme` `cache` that spans across multiple calls and perhaps some memory optimizations.
//! Any non `_recursive` function also uses a per `scheme` `cache`, however it is dropped after the function call.
//! This essentially means that the resolver function will not be called twice for the same placeholder token. The added `depth` parameter works as follows:
//! - if `depth = 0` returns `input`.
//! - if `depth = 1` the result is the same as if calling [`resolve`][fn@resolve] once.
//! - if `depth > 1` the result is the same as if calling [`resolve`][fn@resolve] `depth` times by passing in as `input` it's own output with the added benefit of a cache.
//!
//! # Resolver function
//! In order to use this module you will need to provide a `resolver function` which is a function that understands a certain scheme and resolves a key to an actual value.
//! - `F` is the resolver function which is an `FnMut` closure taking a `scheme` and a `key` as parameters, these are parsed from a token `${scheme:key}`.
//! - `F` is generic over `E` thus it can return any error it wishes. Should it return an error, the same error is propagated to the resolve function which in turn will return it to the caller.
//! - If `F` does not recognize the `scheme` it should return `Ok(None)` indicating that the placeholder should not be replaced.
//! - If `F` does recognize the `scheme` it should return `Ok(Some(value))` where `value` is the value of the key in the given recognized scheme.
//! - Should `F` encounter an error while retrieving the value for a `key` in a given known `scheme` it can either forward the error to `resolve` (by returning `Err(retrieve_error)`) which will itself forward it to it's caller or it can return `Ok(None)` indicating that the placeholder should not be replaced.
//!
//! `F` is only called once per `scheme:key` pair by `resolve`, the results are cached, other occurrences of the same `scheme:key` will be handled by cached values.
//!
//! The resolver closure looks as follows:
//! ```
//! fn resolver<E>(scheme : &str, key : &str) -> Result<Option<String>, E>
//! where
//!     E: std::error::Error {
//! Ok(None)
//! }
//! ```
//! # Returns
//! The `resolve` functions are usually generic over `E` which denotes a possible error returned by the `resolver` function;
//! should the resolver function return a `Result:Err` the resolve function will forward it to the caller by returning the same error.
//!
//! Otherwise it returns `OK(resolved_input)` where `resolved_input` is the same `input` string passed as the `input` parameter with all occurrences of all placeholders replaced with values returned by the resolver function.

pub use self::error::VarError;

use std::collections::HashMap;

/// Iteratively calls [`resolve_from_env`][fn@resolve_from_env] with the same `cache` up to `depth` times.
/// See [`module`][mod@self] level docs docs.
///
/// Use this instead of [`resolve_from_env`][fn@resolve_from_env] if the environment variable values still contain `${scheme:key}` placeholders having the same `match_scheme`, those will be further replaced by other environment variables up to `depth` times.
///
/// # Parameters
/// * `input` - The string to be interpolated containing `${scheme:key}` placeholders.
/// * `depth` - The amount of times the [`resolve_from_env`][fn@resolve_from_env] function should be called by feeding in it's own output.
/// * `match_scheme` - Which scheme should the environment variable resolver function match against ?, "env" is recommended.
///
/// The resolver function will call [`std::env::var`][fn@std::env::var] mapping any error to the custom [`error::VarError`][enum@error::VarError] error type which includes the key that was requested.
///
/// # Returns
/// The interpolated `input` string with all placeholders matching `match_scheme` scheme replaced by the value found in the environment variables given the placeholders key.
/// Otherwise a [`error::VarError`][enum@error::VarError]
///
/// # Usage
/// ```
/// # use tracing_config::interpolate::resolve_from_env_recursive;
/// # fn f() -> Result<(), Box<dyn std::error::Error>> {
/// let input = "Hello ${env:username}";
/// let greeting = resolve_from_env_recursive(input, 5, "env")?;
/// println!("greeting : {}", greeting);
/// # Ok(()) }
/// ```
pub fn resolve_from_env_recursive(
    input: &str,
    depth: u8,
    match_scheme: &str,
) -> Result<String, self::error::VarError> {
    resolve_recursive::<self::error::VarError, _>(input, depth, |scheme, key| {
        if scheme == match_scheme {
            Ok(Some(std::env::var(key).map_err(|err| {
                self::error::VarError::from_std(key, err)
            })?))
        } else {
            Ok(None)
        }
    })
}

/// Performs environment variable placeholder `${scheme:key}` replacement given an `input` string and returns the interpolated `input` string.
/// See [`module`][mod@self] level docs docs.
///
/// This calls [`resolve`][fn@resolve] with an [`error::VarError`][enum@error::VarError] error type.
/// The resolver function `F` will call [`std::env::var`][fn@std::env::var] mapping any error to the custom [`error::VarError`][enum@error::VarError] error type which includes the key that was requested.
///
/// Use this instead of [`resolve`][fn@resolve] if you wish to resolve environment variables.
///
/// The `match_scheme` parameter gives you freedom to select any scheme you want other than the obvious `env` scheme.
///
/// # Parameters
///
/// * `input` - The string to be interpolated containing `${scheme:key}` placeholders.
/// * `match_scheme` - The scheme of the placeholder for example `env` (placeholders would look like `${env:key}`)
///
/// # Returns
///
/// The interpolated `input` string with all placeholders matching `match_scheme` scheme replaced by the value found in the environment variables given the placeholders key.
/// Otherwise a [`error::VarError`][enum@error::VarError]
///
/// # Usage
/// ```
/// # use tracing_config::interpolate::resolve_from_env;
/// # fn f() -> Result<(), Box<dyn std::error::Error>> {
/// let input = "Hello ${env:username}";
/// let greeting = resolve_from_env(input, "env")?;
/// println!("greeting : {}", greeting);
/// # Ok(()) }
/// ```
pub fn resolve_from_env(input: &str, match_scheme: &str) -> Result<String, self::error::VarError> {
    resolve::<self::error::VarError, _>(input, |scheme, key| {
        if scheme == match_scheme {
            Ok(Some(std::env::var(key).map_err(|err| {
                self::error::VarError::from_std(key, err)
            })?))
        } else {
            Ok(None)
        }
    })
}

/// Iteratively calls [`resolve_infallible`][fn@resolve_infallible] with the same `cache` up to `depth` times.
/// See [`module`][mod@self] level docs docs.
///
/// This calls [`resolve_recursive`][fn@resolve_recursive] with an [`std::convert::Infallible`][enum@std::convert::Infallible] error type for the resolver function `F`.
///
/// Use this instead of [`resolve_infallible`][fn@resolve_infallible] if your resolver function `F` never fails.
///
/// # Generics
/// * `F` - Resolver function
///
/// # Parameters
/// * `input` - The string to be interpolated containing `${scheme:key}` placeholders.
/// * `depth` - The amount of times the [`resolve`][fn@resolve] function should be called by feeding in it's own output.
/// * `resolver` - The resolver function, is a closure taking `scheme` and `key`, given `scheme` it should return a value for `key`.
///
/// # Returns
/// `interpolated_input`
pub fn resolve_infallible_recursive<F>(input: &str, depth: u8, mut resolver: F) -> String
where
    F: FnMut(&str, &str) -> Option<String>,
{
    resolve_recursive::<std::convert::Infallible, _>(input, depth, |scheme, key| {
        Ok(resolver(scheme, key))
    })
    .expect("infallible operation")
}

/// Perform `${scheme:key}` placeholder replacement given an `input` string.
/// See [`module`][mod@self] level docs docs.
///
/// This calls [`resolve`][fn@resolve] with an [`std::convert::Infallible`][enum@std::convert::Infallible] error type for the resolver function `F`.
///
/// Use this instead of [`resolve`][fn@resolve] if your resolver function `F` never fails.
///
/// # Generics
/// * `F` - Resolver function
///
/// # Parameters
/// * `input` - The string to be interpolated containing `${scheme:key}` placeholders.
/// * `resolver` - The resolver function, is a closure taking `scheme` and `key`, given `scheme` it should return a value for `key`.
///
/// # Returns
/// - `Ok`(`interpolated_input`);
/// - `Err`(`resolver_error`) In case `resolver` returns an `Err`.
///
/// # Usage
/// ```
/// # use std::collections::HashMap;
/// # use tracing_config::interpolate::*;
/// let mut map = HashMap::new();
/// map.insert("username".to_owned(), "John".to_owned());
///
/// let input = "Hello ${hashmap:username}";
/// let greeting = resolve_infallible(input, |scheme, key| {
///     if scheme == "hashmap" {
///         return match map.get(key) {
///             Some(value) => Some(value.clone()),
///             None => None,
///         }
///     }
///     None
/// });
/// println!("greeting : {}", greeting);
/// ```
pub fn resolve_infallible<F>(input: &str, mut resolver: F) -> String
where
    F: FnMut(&str, &str) -> Option<String>,
{
    resolve::<std::convert::Infallible, _>(input, |scheme, key| Ok(resolver(scheme, key)))
        .expect("infallible operation")
}

/// Iteratively calls [`resolve`][fn@resolve] with the same `cache` up to `depth` times.
/// See [`module`][mod@self] level docs docs.
///
/// Use this instead of [`resolve`][fn@resolve] if the values returned by the resolver function still contain `${scheme:key}` placeholders that need to be processed by the same resolver function.
///
/// # Parameters
/// * `input` - The string to be interpolated containing `${scheme:key}` placeholders.
/// * `depth` - The amount of times the [`resolve`][fn@resolve] function should be called by feeding in it's own output.
/// * `resolver` - The resolver function, is a closure taking `scheme` and `key`, given `scheme` it should return a value for `key`.
///
/// # Returns
/// The interpolated `input` string with all placeholders replaced by the value returned by the resolver function.
///
/// # Usage
/// ```
/// let mut map = HashMap::new();
/// // make sure you avoid these cases.
/// // map.insert("username".to_owned(), "[John ${hashmap:username} ${hashmap:code}]".to_owned());
/// // map.insert("code".to_owned(), "(Red ${hashmap:username})".to_owned());
///
/// map.insert(
///     "username".to_owned(),
///     "Username contains a placeholder {${hashmap:actual_name}}".to_owned(),
/// );
/// map.insert("actual_name".to_owned(), "John".to_owned());
/// map.insert("code".to_owned(), "Red".to_owned());
///
/// map.insert("1".to_owned(), "${hashmap:2}".to_owned());
/// map.insert("2".to_owned(), "${hashmap:3}".to_owned());
/// map.insert("3".to_owned(), "${hashmap:4}".to_owned());
/// map.insert("4".to_owned(), "${hashmap:5}".to_owned());
/// map.insert("5".to_owned(), "${hashmap:6}".to_owned());
/// map.insert("6".to_owned(), "${hashmap:7}".to_owned());
/// map.insert("7".to_owned(), "Seven".to_owned());
///
/// let input = "Hello \"${hashmap:username}\", code is ${hashmap:code}, depth is := ${hashmap:1}.";
/// let greeting = resolve_recursive::<std::convert::Infallible, _>(input, 7, |scheme, key| {
///     println!("resolver called for : {scheme}:{key}");
///     if scheme == "hashmap" {
///         return match map.get(key) {
///             Some(value) => Ok(Some(value.clone())),
///             None => Ok(None),
///         };
///     }
///     Ok(None)
/// })
/// // it's safe to unwrap infallible
/// .unwrap();
///
/// println!("Recursively resolved input : {}", greeting);
/// ```
pub fn resolve_recursive<E, F>(input: &str, mut depth: u8, mut resolver: F) -> Result<String, E>
where
    F: FnMut(&str, &str) -> Result<Option<String>, E>,
    E: std::error::Error,
{
    match depth {
        0 => Ok(input.to_owned()),
        1 => Ok(resolve_private(input, &mut HashMap::new(), &mut resolver)?.0),
        _ => {
            let mut cache: HashMap<String, HashMap<String, Option<String>>> = HashMap::new();
            let (mut result, mut resolver_useful) =
                resolve_private(input, &mut cache, &mut resolver)?;
            depth -= 1;
            while resolver_useful && depth >= 1 {
                (result, resolver_useful) =
                    resolve_private(result.as_str(), &mut cache, &mut resolver)?;
                depth -= 1;
            }
            Ok(result)
        }
    }
}

/// Perform `${scheme:key}` placeholder replacement given an `input` string.
/// See [`module`][mod@self] level docs docs.
///
/// # Generics
/// * `E` - Resolver error
/// * `F` - Resolver function
///
/// # Parameters
/// * `input` - The string to be interpolated containing `${scheme:key}` placeholders.
/// * `resolver` - The resolver function, is a closure taking `scheme` and `key`, given `scheme` it should return a value for `key`.
///
/// # Returns
/// - `Ok`(`interpolated_input`);
/// - `Err`(`resolver_error`) In case `resolver` returns an `Err`.
///
/// # Usage Basic
///
/// `NOTE:` there is a dedicated helper function that resolves environment variables [`resolve_from_env`][fn@resolve_from_env]. This is just a basic usage example.
/// ```
/// // simple env var resolver
/// let input = "Hello ${env:username}";
/// let greeting = resolve::<StdVarError, _>(input, |scheme, key| {
///     if scheme == "env" {
///         Ok(Some(
///             std::env::var(key)?,
///         ))
///     } else {
///         Ok(None)
///     }
/// });
///
/// match greeting {
///     Ok(greeting) => println!("{}", greeting),
///     Err(var_error) => println!("Could not greet user since `username` environment variable is either not set or non unicode. The error is : {:?}", var_error),
/// }
/// ```
/// # Usage Advanced
///
/// ```
/// // This is the original input, we have to replace ${http:username} by calling a server
/// let input = "Hello ${http:username}, have a nice day. The following is a placeholder that should not be replaced : ${scheme:key}. We reiterate that your name is : ${http:username}";
/// println!("original input = {}", input);
/// // we specify that our resolver function could error with an http::HttpError
/// let input = resolve::<http::HttpError, _>(
///     input,
///     // our http resolver closure takes in the scheme and the key, it will be called once for ${http:username} and once for ${scheme:key}
///     |scheme, key| {
///         // the resolver function checks for known schemes
///         if scheme == "http" {
///             // we call our http service to get the value of the given key, in the example case is username
///             // we can either return the http error, or decide to not replace the value by returning Ok(None), in this case, should there be one, we decided to return an error.
///             let value = http::get(format!("http://localhost:8080/variables/{}", key))?;
///             // if we were successfully able to return a value we return Ok(Some(Value))
///             // to indicate that the placeholder should be replaced
///             // in the example, given that there are 2 occurrences of ${http:username}, this closure will be called only once for scheme = http and key = username
///             return Ok(Some(value));
///         }
///         // for unknown schemes, we should return Ok(None) to indicate that we do not want to resolve or replace the given key in the given scheme
///         // in the example this is ${scheme:key}
///         Ok(None)
///     },
/// );
///
/// // if the closure recognized the http scheme, tried to call the service to retrieve the value of the username key,
/// // was unsuccessful, and decided that it should error, this is the error returned by the closure.
/// // in many cases you would simply error out and return it to the caller.
/// let input = match input {
///     Ok(ok) => ok,
///     Err(http_error) => return Err(From::from(http_error)),
/// };
///
/// println!("resolved input = {}", input);
/// ```
pub fn resolve<E, F>(input: &str, mut resolver: F) -> Result<String, E>
where
    F: FnMut(&str, &str) -> Result<Option<String>, E>,
    E: std::error::Error,
{
    Ok(resolve_private(input, &mut HashMap::new(), &mut resolver)?.0)
}

/// Perform `${scheme:key}` placeholder replacement given an `input` string.
/// See [`module`][mod@self] level docs docs.
///
/// # Generics
/// * `E` - Resolver error
/// * `F` - Resolver function
///
/// # Parameters
/// * `input` - The string to be interpolated containing `${scheme:key}` placeholders.
/// * `cache` - A resolver cache per scheme.
/// * `resolver` - The resolver function, is a closure taking `scheme` and `key`, given `scheme` it should return a value for `key`.
///
/// # Returns
/// - `Ok`(`interpolated_input`, `resolver_useful`); `resolver_useful` is `true` if `resolver` returned `Ok(Some(value))`
/// - `Err`(`resolver_error`) In case `resolver` returns an `Err`.
fn resolve_private<E, F>(
    input: &str,
    cache: &mut HashMap<String, HashMap<String, Option<String>>>,
    resolver: &mut F,
) -> Result<(String, bool), E>
where
    F: FnMut(&str, &str) -> Result<Option<String>, E>,
    E: std::error::Error,
{
    const DOLLAR: char = '$';
    const COLON: char = ':';
    const OPEN_CURLY: char = '{';
    const CLOSE_CURLY: char = '}';

    // parser state machine
    enum State {
        Normal,
        SchemeStart,
        Scheme,
        Key { scheme: String },
    }

    let mut resolver_useful = false;
    let mut state = State::Normal;
    let mut result = String::with_capacity(input.len());
    let mut temp = String::new(); // Temporary storage // with capacity ?

    for c in input.chars() {
        match &state {
            State::Normal => match c {
                DOLLAR => state = State::SchemeStart,
                _ => result.push(c),
            },
            State::SchemeStart => match c {
                OPEN_CURLY => state = State::Scheme,
                _ => {
                    result.push(DOLLAR);
                    result.push(c);
                    state = State::Normal;
                }
            },
            State::Scheme => match c {
                COLON => {
                    state = State::Key {
                        scheme: temp.clone(),
                    };
                    temp.clear();
                }
                _ => temp.push(c),
            },
            State::Key { scheme } => match c {
                CLOSE_CURLY => {
                    use std::collections::hash_map::Entry::Occupied;
                    use std::collections::hash_map::Entry::Vacant;

                    let key = &temp;

                    // get values cache for scheme
                    let cache = cache.entry(scheme.clone()).or_default();

                    let value: &Option<String> = match cache.entry(key.clone()) {
                        Occupied(entry) => entry.into_mut(),
                        Vacant(entry) => {
                            let resolver_result = resolver(scheme, key)?;
                            if resolver_result.is_some() {
                                resolver_useful = true;
                            }
                            entry.insert(resolver_result)
                        }
                    };

                    match value {
                        Some(value) => result.push_str(value),
                        None => {
                            result.push(DOLLAR);
                            result.push(OPEN_CURLY);
                            result.push_str(scheme);
                            result.push(COLON);
                            result.push_str(key);
                            result.push(CLOSE_CURLY);
                        }
                    }
                    temp.clear();
                    state = State::Normal;
                }
                _ => temp.push(c),
            },
        }
    }

    // maybe if we're not in the normal state by the time we finish parsing we should error ?
    match &state {
        State::Normal => {}
        State::SchemeStart => {
            result.push(DOLLAR);
        }
        State::Scheme => {
            result.push(DOLLAR);
            result.push(OPEN_CURLY);
            result.push_str(&temp);
        }
        State::Key { scheme } => {
            result.push(DOLLAR);
            result.push(OPEN_CURLY);
            result.push_str(scheme);
            result.push(COLON);
            result.push_str(&temp);
        }
    }

    Ok((result, resolver_useful))

    // #states
    // Normal
    // SchemeStart
    // Scheme
    // Key

    // #initial
    // Normal

    // #accepting
    // Normal

    // #alphabet
    // DOLLAR
    // COLON
    // OPEN_CURLY
    // CLOSE_CURLY
    // NOT_DOLLAR
    // NOT_COLON
    // NOT_OPEN_CURLY
    // NOT_CLOSE_CURLY

    // #transitions
    // Normal:DOLLAR>SchemeStart
    // SchemeStart:OPEN_CURLY>Scheme
    // Scheme:COLON>Key
    // Key:CLOSE_CURLY>Normal
    // Normal:NOT_DOLLAR>Normal
    // SchemeStart:NOT_OPEN_CURLY>Normal
    // Scheme:NOT_COLON>Scheme
    // Key:NOT_CLOSE_CURLY>Key
}

/// Interpolation for [`toml`][mod@::toml] [`Value`][enum@::toml::Value].
pub mod toml {
    use super::error::VarError;
    use toml::Value::{
        self as TomlValue, Boolean as TomlBool, Integer as TomlInt, Float as TomlFloat,
        Datetime as TomlDateTime, String as TomlString, Table as TomlTable, Array as TomlArray,
    };

    /// Recursively replaces any `${env:key}` placeholders with environment variable values in any toml [`Value`][enum@TomlValue] where the value is a toml [`String`][type@TomlString].
    /// See the [`resolve_from_env_recursive`][fn@super::resolve_from_env_recursive] function.
    pub fn resolve_from_env_recursive(value: &mut TomlValue, depth: u8) -> Result<(), VarError> {
        match value {
            TomlBool(_) => Ok(()),
            TomlInt(_) => Ok(()),
            TomlFloat(_) => Ok(()),
            TomlDateTime(_) => Ok(()),
            TomlString(str_val) => {
                let replaced = super::resolve_from_env_recursive(str_val, depth, "env")?;
                *value = TomlString(replaced);
                Ok(())
            }
            TomlTable(map) => {
                for (_key, val) in map.iter_mut() {
                    resolve_from_env_recursive(val, depth)?;
                }
                Ok(())
            }
            TomlArray(array) => {
                for val in array.iter_mut() {
                    resolve_from_env_recursive(val, depth)?;
                }
                Ok(())
            }
        }
    }
}

/// Interpolation errors
mod error {
    use std::env::VarError as StdVarError;
    use std::error::Error as StdError;
    use std::fmt::Result as FmtResult;
    use std::ffi::OsString;
    use std::fmt::Display;
    use std::fmt::Formatter;

    /// This is the same error type as defined in [`std::env::VarError`][enum@StdVarError] with the added originally requested `key`.
    #[derive(Debug)]
    pub enum VarError {
        NotPresent { key: String },
        NotUnicode { key: String, value: OsString },
    }

    impl StdError for VarError {}

    impl Display for VarError {
        fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
            match &self {
                VarError::NotPresent { key } => write!(f, "environment variable `{key}` not found"),
                VarError::NotUnicode { key, ref value } => {
                    write!(
                        f,
                        "environment variable `{key}` was not valid unicode: {:?}",
                        value
                    )
                }
            }
        }
    }

    impl VarError {
        pub fn from_std(key: &str, err: StdVarError) -> Self {
            match err {
                StdVarError::NotPresent => Self::NotPresent {
                    key: key.to_owned(),
                },
                StdVarError::NotUnicode(os_str) => Self::NotUnicode {
                    key: key.to_owned(),
                    value: os_str,
                },
            }
        }
    }
}
