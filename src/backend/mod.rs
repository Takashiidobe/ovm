pub mod x86_64;

use crate::frontend::parser::Expr;

pub trait Backend {
    fn generate_assembly(&self, expr: &Expr) -> String;
}
