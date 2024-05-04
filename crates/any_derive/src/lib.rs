extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields};

#[proc_macro_derive(Any)]
pub fn validate_macro_derive(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);

    let id = input.ident;

    let exp = match input.data {
        Data::Enum(de) => {
            let mut variants = vec![];

            for (i, variant) in de.variants.iter().enumerate() {
                let var_id = &variant.ident;

                match &variant.fields {
                    Fields::Named(_f) => {
                        panic!("not supported")
                    }
                    Fields::Unnamed(_) => {
                        panic!("not supported")
                    }
                    Fields::Unit => {
                        variants.push(quote!(#i => #id::#var_id,));
                    }
                }
            }

            variants
        }
        _ => {
            panic!("not supported")
        }
    };

    let expanded = quote!(
        impl symex_lib::Any for #id {
            fn any() -> Self {
                let n = u8::any();
                match n {
                    #(#exp)*
                    _ => symex_lib::ignore_path(),
                }
            }
        }
    );
    proc_macro::TokenStream::from(expanded)
}
