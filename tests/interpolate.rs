use std::collections::HashMap;
use tracing_config::interpolate::*;

use tracing_config::interpolate::VarError;

#[cfg(test)]
mod http {
    #[derive(Debug, thiserror::Error)]
    pub enum HttpError {
        #[error("SomeError")]
        SomeError,
    }
    pub fn get(s: String) -> Result<String, HttpError> {
        if s == "unknown" {
            return Err(HttpError::SomeError);
        }
        Ok("John Doe".to_owned())
    }
}

#[test]
fn test_http_resolver() -> Result<(), self::http::HttpError> {
    // This is the original input, we have to replace ${http:username} by calling a server
    let input = "Hello ${http:username}, have a nice day. The following is a placeholder that should not be replaced : ${s:k}. We reiterate that your name is : ${http:username}";
    println!("original input = {}", input);
    // we specify that our resolver function could error with an http::HttpError
    let input = resolve::<self::http::HttpError, _>(
        input,
        // our http resolver closure takes in the scheme and the key, it will be called once for ${http:username} and once for ${s:k}
        |scheme, key| {
            // the resolver function checks for known schemes
            if scheme == "http" {
                // we call our http service to get the value of the given key, in the example case is username
                // we can either return the http error, or decide to not replace the value by returning Ok(None), in this case, should there not be one, we decided to return an error.
                let value = self::http::get(format!("http://localhost:8080/variables/{}", key))?;
                // if we were successfully able to return a value we return Ok(Some(Value))
                // to indicate that the placeholder should be replaced
                // in the example, given that there are 2 occurrences of ${http:username}, this closure will be called only once for scheme = http and key = username
                return Ok(Some(value));
            }
            // for unknown schemes, we should return Ok(None) to indicate that we do not want to resolve or replace the given key in the given scheme
            // in the example this is ${s:k}
            Ok(None)
        },
    );

    // if the closure recognized the http scheme, tried to call the service to retrieve the value of the username key,
    // was unsuccessful, and decided that it should error, this is the error returned by the closure.
    // in many cases you would simply error out and return it to the caller.
    let input = match input {
        Ok(ok) => ok,
        Err(http_error) => return Err(From::from(http_error)),
    };

    println!("resolved input = {}", input);
    assert_eq!(input, "Hello John Doe, have a nice day. The following is a placeholder that should not be replaced : ${s:k}. We reiterate that your name is : John Doe");

    Ok(())
}

#[test]
fn test_greet_user() {
    // simple env var resolver
    let input = "Hello ${env:username}";
    let greeting = resolve::<std::env::VarError, _>(input, |scheme, key| {
        if scheme == "env" {
            Ok(Some(std::env::var(key)?))
        } else {
            Ok(None)
        }
    });

    match greeting {
        Ok(greeting) => println!("{}", greeting),
        Err(var_error) => println!("Could not greet user since `username` environment variable is either not set or non unicode. The error is : {:?}", var_error),
    }
}

#[test]
fn test_resolve_env() -> Result<(), VarError> {
    let input = "Hello ${env:username}";
    let greeting = resolve_from_env(input, "env")?;
    println!("greeting : {}", greeting);
    Ok(())
}

#[test]
fn test_resolve_inf() -> Result<(), VarError> {
    let mut map = HashMap::new();
    map.insert("username".to_owned(), "John".to_owned());

    let input = "Hello ${hashmap:username}";
    let greeting = resolve_infallible(input, |scheme, key| {
        if scheme == "hashmap" {
            return match map.get(key) {
                Some(value) => Some(value.clone()),
                None => None,
            };
        }
        None
    });
    println!("greeting : {}", greeting);
    Ok(())
}

#[test]
fn test_resolve_from_env_recursive() -> Result<(), VarError> {
    // to test, run with the following environment variables :
    // - fs_tracing_config = ${env:app_config}/tracing.toml
    // - app_config = ${env:app_home}/config
    // - app_home = /some/path
    // you should get /some/path/config/tracing.toml
    let input = "Full config path is : ${env:fs_tracing_config}";
    let greeting = resolve_from_env_recursive(input, 5, "env")?;
    println!("greeting : {}", greeting);
    Ok(())
}

#[test]
fn test_resolve_recursive() -> Result<(), VarError> {
    let mut map = HashMap::new();
    // make sure you avoid these cases.
    // map.insert("username".to_owned(), "[John ${hashmap:username} ${hashmap:code}]".to_owned());
    // map.insert("code".to_owned(), "(Red ${hashmap:username})".to_owned());

    map.insert(
        "username".to_owned(),
        "Username contains a placeholder {${hashmap:actual_name}}".to_owned(),
    );
    map.insert("actual_name".to_owned(), "John".to_owned());
    map.insert("code".to_owned(), "Red".to_owned());

    map.insert("1".to_owned(), "${hashmap:2}".to_owned());
    map.insert("2".to_owned(), "${hashmap:3}".to_owned());
    map.insert("3".to_owned(), "${hashmap:4}".to_owned());
    map.insert("4".to_owned(), "${hashmap:5}".to_owned());
    map.insert("5".to_owned(), "${hashmap:6}".to_owned());
    map.insert("6".to_owned(), "${hashmap:7}".to_owned());
    map.insert("7".to_owned(), "Seven".to_owned());

    let input = "Hello \"${hashmap:username}\", code is ${hashmap:code}, depth is := ${hashmap:1}.";
    let greeting = resolve_recursive::<std::convert::Infallible, _>(input, 7, |scheme, key| {
        println!("resolver called for : {scheme}:{key}");
        if scheme == "hashmap" {
            return match map.get(key) {
                Some(value) => Ok(Some(value.clone())),
                None => Ok(None),
            };
        }
        Ok(None)
    })
    // it's safe to unwrap infallible
    .unwrap();

    println!("Recursively resolved input : {}", greeting);
    Ok(())
}
