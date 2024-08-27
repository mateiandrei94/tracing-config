#![forbid(unsafe_code)]
#![warn(missing_docs)]
#![warn(rustdoc::private_intra_doc_links)]
#![doc = include_str!("../README.md")]

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemFn};

/// todo
#[proc_macro_attribute]
pub fn test(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let mut org_fn = parse_macro_input!(item as ItemFn);

    // Get the original function name
    let org_fn_ident: syn::Ident = org_fn.sig.ident.clone();
    let org_fn_ident_string = org_fn_ident.to_string();
    let org_fn_ident_span = org_fn_ident.span().clone();
    org_fn.sig.ident = syn::Ident::new(
        format!("{org_fn_ident_string}_private").as_ref(),
        org_fn_ident_span,
    );
    let private_fn = &org_fn.sig.ident;

    // Generate the new code
    let expanded = quote! {
        fn #org_fn_ident() {
            tracing_config::init!();
            #org_fn;
            #private_fn()
        }
    };

    // Convert the expanded code back into a TokenStream
    TokenStream::from(expanded)
}
