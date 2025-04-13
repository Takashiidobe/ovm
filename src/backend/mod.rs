pub mod x86_64;

use crate::frontend::parser::{Expr, Program};

pub trait Backend {
    fn generate_assembly(&self, program: &Program) -> String;
    fn generate_expr_code(&self, expr: &Expr) -> String;
}
