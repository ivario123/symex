//! Defines a transpiler that allows inline pseudo code
//! to be translated in to [`general_assembly`]
extern crate proc_macro;

use language::ast::IR;
use proc_macro::TokenStream;
use syn::parse_macro_input;

#[proc_macro]
pub fn pseudo(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as IR);
    let input: proc_macro2::TokenStream = input.into();

    input.into()
}
