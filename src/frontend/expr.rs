use crate::{
    error::Error,
    frontend::token::{Object, Token},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Assign {
        name: Token,
        value: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Get {
        object: Box<Expr>,
        name: Token,
    },
    Grouping {
        expr: Box<Expr>,
    },
    Literal {
        value: Object,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Variable {
        name: Token,
    },
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
    },
    Set {
        object: Box<Expr>,
        name: Token,
        value: Box<Expr>,
    },
    Super {
        keyword: Token,
        method: Token,
    },
    This {
        keyword: Token,
    },
}

pub mod expr {
    use crate::{
        error::Error,
        frontend::token::{Object, Token},
    };

    use super::Expr;

    pub trait Visitor<R> {
        fn visit_binary_expr(
            &mut self,
            left: &Expr,
            operator: &Token,
            right: &Expr,
        ) -> Result<R, Error>;
        fn visit_grouping_expr(&mut self, expr: &Expr) -> Result<R, Error>;
        fn visit_literal_expr(&self, value: &Object) -> Result<R, Error>;
        fn visit_unary_expr(&mut self, operator: &Token, right: &Expr) -> Result<R, Error>;
        fn visit_variable_expr(&mut self, name: &Token) -> Result<R, Error>;
        fn visit_assign_expr(&mut self, name: &Token, value: &Expr) -> Result<R, Error>;
        fn visit_logical_expr(
            &mut self,
            left: &Expr,
            operator: &Token,
            right: &Expr,
        ) -> Result<R, Error>;
        fn visit_call_expr(
            &mut self,
            callee: &Expr,
            paren: &Token,
            arguments: &[Expr],
        ) -> Result<R, Error>;
        fn visit_get_expr(&mut self, object: &Expr, name: &Token) -> Result<R, Error>;
        fn visit_set_expr(&mut self, object: &Expr, name: &Token, value: &Expr)
        -> Result<R, Error>;
        fn visit_this_expr(&mut self, keyword: &Token) -> Result<R, Error>;
        fn visit_super_expr(&mut self, keyword: &Token, method: &Token) -> Result<R, Error>;
    }
}

impl Expr {
    pub fn accept<R>(&self, visitor: &mut dyn expr::Visitor<R>) -> Result<R, Error> {
        match self {
            Expr::Assign { name, value } => visitor.visit_assign_expr(name, value),
            Expr::Binary {
                left,
                operator,
                right,
            } => visitor.visit_binary_expr(left, operator, right),
            Expr::Grouping { expr } => visitor.visit_grouping_expr(expr),
            Expr::Literal { value } => visitor.visit_literal_expr(value),
            Expr::Unary { operator, right } => visitor.visit_unary_expr(operator, right),
            Expr::Variable { name } => visitor.visit_variable_expr(name),
            Expr::Logical {
                left,
                operator,
                right,
            } => visitor.visit_logical_expr(left, operator, right),
            Expr::Call {
                callee,
                paren,
                arguments,
            } => visitor.visit_call_expr(callee, paren, arguments),
            Expr::Get { object, name } => visitor.visit_get_expr(object, name),
            Expr::Set {
                object,
                name,
                value,
            } => visitor.visit_set_expr(object, name, value),
            Expr::This { keyword } => visitor.visit_this_expr(keyword),
            Expr::Super { keyword, method } => visitor.visit_super_expr(keyword, method),
        }
    }
}
