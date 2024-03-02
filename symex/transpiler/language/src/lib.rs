use ast::{IRExpr, IdentOperand};
use quote::format_ident;
use syn::Ident;

pub mod ast;
pub mod ga_backend;
pub mod parser;

struct CompilerState<T> {
    to_declare: Vec<Ident>,
    to_insert_above: Vec<T>,
    intermediate_counter: usize,
}

trait Compile {
    type Output;
    fn compile(&self, state: &mut CompilerState<Self::Output>) -> Self::Output;
}

impl<T> CompilerState<T> {
    fn new() -> Self {
        Self {
            to_declare: Vec::new(),
            to_insert_above: Vec::new(),
            intermediate_counter: 0,
        }
    }
    fn intermediate(&mut self) -> Ident {
        let new_ident = format_ident!("intermediate_{}", self.intermediate_counter);
        self.to_declare.push(new_ident.clone());
        self.intermediate_counter += 1;
        new_ident
    }
}
