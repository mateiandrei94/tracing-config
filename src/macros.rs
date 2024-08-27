//! Contains public macros, to be exported in the crate root.

#[doc = include_str!("../doc/config_initialize.md")]
///
#[doc = include_str!("../doc/reference_links.md")]
#[macro_export]
macro_rules! init {
    (
        $($key:ident : $values:expr),*
        $(,)?
    ) => {
        // #[allow(unused_mut, unused_assignments)]
        let _tcg : Option<$crate::config::TracingConfigGuard> = {
            const VERBOSITY_ENV_KEY : &str = "tracing_config_verbosity";
            let mut qualifier : &str = "";
            let mut organization : &str = "";
            let mut name : &str = env!("CARGO_PKG_NAME");
            let mut test : bool = cfg!(test);
            let mut verbosity : Option<$crate::config::model::Level> = std::env::var(VERBOSITY_ENV_KEY)
                .map(|val|  {
                    let val_parsed = $crate::config::model::Level::try_from(val.as_ref())
                        .map(|ok| Some(ok))
                        .unwrap_or(Some($crate::config::model::Level::Warn));
                    if val == "none" {
                        None
                    } else {
                        val_parsed
                    }
                })
                .unwrap_or(Some($crate::config::model::Level::Warn));
            let mut env: Option<&str> = None;
            let mut path: Option<&std::path::Path> = None;
            let mut config: Option<$crate::config::model::TracingConfig> = None;

            #[allow(unused_macros)] // For some reason the compiler emits an "unused macro definition" warning for `set_var` (I think this might be a bug).
            macro_rules! set_var {
                (qualifier, $value:expr) => {
                    qualifier = $value
                };
                (organization, $value:expr) => {
                    organization = $value
                };
                (name, $value:expr) => {
                    name = $value
                };
                (test, $value:expr) => {
                    test = $value
                };
                (verbosity, $value:expr) => {
                    if let Some(new_verbosity) = $crate::config::model::Level::try_from($value)
                        .map(|ok| Some(ok))
                        .unwrap_or(None) {
                            verbosity = Some(new_verbosity)
                        }
                    if $value == "none" {
                        verbosity = None;
                    }
                };
                (env, $value:expr) => {
                    env = Some($value)
                };
                (path, $value:expr) => {
                    path = Some($value)
                };
                (config, $value:expr) => {
                    config = Some($value)
                };
                ($param_key:ident, $value:expr) => {
                    compile_error!(
                        concat!("unexpected argument \"", stringify!($param_key), "\" acceptable parameters are : \
                        \"qualifier\": &str, \
                        \"organization\": &str, \
                        \"name\": &str, \
                        \"test\": bool, \
                        \"verbosity\": &str (\"trace\", \"debug\", \"info\", \"warn\", \"error\", \"none\"), \
                        \"env\": &str, \
                        \"path\": &Path, \
                        \"config\": &TracingConfig")
                    );
                }
            }

            $(
                set_var!($key, $values); // I literally just use it here !
            )*

            let init_result = $crate::config::initialize(qualifier, organization, name, test, verbosity, env, path, config);

            match init_result {
                Ok(guard) => Some(guard),
                Err(init_error) => {
                    if matches!(init_error, $crate::TracingConfigError::AlreadyInitialized) {
                        None
                    } else {
                        println!("[tracing-config] init error; : {init_error:#?}");
                        println!("[tracing-config] init error; : for more details, set \
                        environment variable \"{VERBOSITY_ENV_KEY}\" = \"trace\" and retry.");
                        panic!("[tracing-config] init error; : {init_error:#?}");
                    }
                }
            }
        };
    };
}
