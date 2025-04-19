use crate::{
    error::{Error, parser_error},
    frontend::token::{Object, Token, TokenType},
};

use super::{expr::Expr, stmt::Stmt};

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            ..Default::default()
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Error> {
        let mut statements = vec![];
        while !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        Ok(statements)
    }

    pub fn parse_exprs(&mut self) -> Result<Vec<Expr>, Error> {
        let mut expressions = vec![];
        while !self.is_at_end() {
            expressions.push(self.expression()?);
        }
        Ok(expressions)
    }

    fn expression(&mut self) -> Result<Expr, Error> {
        self.assignment()
    }

    fn declaration(&mut self) -> Result<Stmt, Error> {
        let statement = if self.r#match(&[TokenType::Fun]) {
            self.function("function")
        } else if self.r#match(&[TokenType::Var]) {
            self.var_declaration()
        } else if self.r#match(&[TokenType::Class]) {
            self.class_declaration()
        } else if self.r#match(&[TokenType::If]) {
            self.if_statement()
        } else {
            self.statement()
        };

        match statement {
            Err(e) => {
                self.synchronize();
                Err(e)
            }
            stmt => stmt,
        }
    }

    fn class_declaration(&mut self) -> Result<Stmt, Error> {
        let name = self.consume(&TokenType::Identifier, "Expect class name.")?;
        let superclass = if self.r#match(&[TokenType::Less]) {
            self.consume(&TokenType::Identifier, "Expect superclass name.")?;
            Some(self.previous().clone())
        } else {
            None
        };

        self.consume(&TokenType::LeftBrace, "Expect '{' before class body.")?;

        let mut methods = vec![];

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            methods.push(self.function("method")?);
        }

        self.consume(&TokenType::RightBrace, "Expect '}' after class body.")?;

        Ok(Stmt::Class {
            name,
            superclass: superclass.map(|name| Expr::Variable { name }),
            methods,
        })
    }

    fn if_statement(&mut self) -> Result<Stmt, Error> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(&TokenType::RightParen, "Expect ')' after if condition.")?;

        let then_branch = Box::new(self.statement()?);

        let else_branch = Box::new(if self.r#match(&[TokenType::Else]) {
            Some(self.statement()?)
        } else {
            None
        });

        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn statement(&mut self) -> Result<Stmt, Error> {
        if self.r#match(&[TokenType::Print]) {
            self.print_statement()
        } else if self.r#match(&[TokenType::Return]) {
            self.return_statement()
        } else if self.r#match(&[TokenType::For]) {
            self.for_statement()
        } else if self.r#match(&[TokenType::While]) {
            self.while_statement()
        } else if self.r#match(&[TokenType::LeftBrace]) {
            Ok(Stmt::Block {
                statements: self.block()?,
            })
        } else {
            self.expression_statement()
        }
    }

    fn return_statement(&mut self) -> Result<Stmt, Error> {
        let keyword = self.previous();
        let value = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(&TokenType::Semicolon, "Expect ';' after return value.")?;
        Ok(Stmt::Return { keyword, value })
    }

    fn for_statement(&mut self) -> Result<Stmt, Error> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'for'.")?;

        let initializer = if self.r#match(&[TokenType::Semicolon]) {
            None
        } else if self.r#match(&[TokenType::Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let mut condition = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(&TokenType::Semicolon, "Expect ';' after loop condition.")?;

        let increment = if !self.check(&TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(&TokenType::RightParen, "Expect ')' after loop condition.")?;

        let mut body = self.statement()?;

        if let Some(incr) = increment {
            body = Stmt::Block {
                statements: vec![body, Stmt::Expression { expr: incr }],
            };
        }

        if condition.is_none() {
            condition = Some(Expr::Literal {
                value: Object::Bool(true),
            });
        }

        body = Stmt::While {
            condition: condition.unwrap(),
            body: Box::new(body),
        };

        if let Some(init) = initializer {
            body = Stmt::Block {
                statements: vec![init, body],
            }
        }

        Ok(body)
    }

    fn print_statement(&mut self) -> Result<Stmt, Error> {
        let expr = self.expression()?;
        self.consume(&TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::Print { expr })
    }

    fn var_declaration(&mut self) -> Result<Stmt, Error> {
        let name = self.consume(&TokenType::Identifier, "Expect variable name.")?;

        let initializer = if self.r#match(&[TokenType::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(
            &TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;

        Ok(Stmt::Var { name, initializer })
    }

    fn while_statement(&mut self) -> Result<Stmt, Error> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(&TokenType::RightParen, "Expect ')' after condition.")?;
        let body = Box::new(self.statement()?);

        Ok(Stmt::While { condition, body })
    }

    fn expression_statement(&mut self) -> Result<Stmt, Error> {
        let expr = self.expression()?;
        self.consume(&TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::Expression { expr })
    }

    fn function(&mut self, kind: &str) -> Result<Stmt, Error> {
        let name = self.consume(&TokenType::Identifier, &format!("Expect {kind} name."))?;
        self.consume(
            &TokenType::LeftParen,
            &format!("Expect '(' after {kind} name."),
        )?;

        let mut params = vec![];

        if !self.check(&TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    self.error(&self.peek(), "Cannot have more than 255 parameters.");
                }
                params.push(self.consume(&TokenType::Identifier, "Expect parameter name.")?);

                if !self.r#match(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(&TokenType::RightParen, "Expect ')' after parameters.")?;

        self.consume(
            &TokenType::LeftBrace,
            &format!("Expect '{{' before {} body.", kind),
        )?;

        let body = self.block()?;

        Ok(Stmt::Function { name, params, body })
    }

    fn block(&mut self) -> Result<Vec<Stmt>, Error> {
        let mut statements = vec![];

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(&TokenType::RightBrace, "Expect '}' after block.")?;

        Ok(statements)
    }

    fn assignment(&mut self) -> Result<Expr, Error> {
        let expr = self.or()?;

        if self.r#match(&[TokenType::Equal]) {
            let equals = &self.previous();
            let value = Box::new(self.assignment()?);

            match expr {
                Expr::Variable { name } => return Ok(Expr::Assign { name, value }),
                Expr::Get { object, name } => {
                    return Ok(Expr::Set {
                        object,
                        name,
                        value,
                    });
                }
                _ => return Err(self.error(equals, "Invalid assignment target.")),
            }
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr, Error> {
        let mut expr = self.and()?;

        while self.r#match(&[TokenType::Or]) {
            let operator = self.previous();
            let right = Box::new(self.and()?);
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right,
            };
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, Error> {
        let mut expr = self.equality()?;

        while self.r#match(&[TokenType::And]) {
            let operator = self.previous();
            let right = Box::new(self.equality()?);
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right,
            };
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, Error> {
        let mut expr = self.comparison()?;

        while self.r#match(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, Error> {
        let mut expr = self.shift()?;

        while self.r#match(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous();
            let right = self.shift()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn shift(&mut self) -> Result<Expr, Error> {
        let mut expr = self.term()?;

        while self.r#match(&[TokenType::LeftShift, TokenType::RightShift]) {
            let operator = self.previous();
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, Error> {
        let mut expr = self.factor()?;

        while self.r#match(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous();
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, Error> {
        let mut expr = self.unary()?;

        while self.r#match(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, Error> {
        if self.r#match(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.unary()?;
            Ok(Expr::Unary {
                operator,
                right: Box::new(right),
            })
        } else {
            self.call()
        }
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, Error> {
        let mut arguments = vec![];

        if !self.check(&TokenType::RightParen) {
            loop {
                arguments.push(self.expression()?);
                if arguments.len() >= 255 {
                    self.error(&self.peek(), "Can't have more than 255 arguments.");
                }
                if !self.r#match(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        let paren = self.consume(&TokenType::RightParen, "Expect ')' after arguments.")?;

        Ok(Expr::Call {
            callee: Box::new(callee),
            paren,
            arguments,
        })
    }

    fn call(&mut self) -> Result<Expr, Error> {
        let mut expr = self.primary()?;

        loop {
            if self.r#match(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.r#match(&[TokenType::Dot]) {
                let name = self.consume(&TokenType::Identifier, "Expect property after '.'/")?;
                expr = Expr::Get {
                    object: Box::new(expr),
                    name,
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, Error> {
        let token = self.peek();

        let expr = if self.r#match(&[TokenType::False]) {
            Expr::Literal {
                value: Object::Bool(false),
            }
        } else if self.r#match(&[TokenType::True]) {
            Expr::Literal {
                value: Object::Bool(true),
            }
        } else if self.r#match(&[TokenType::This]) {
            Expr::This {
                keyword: self.peek().clone(),
            }
        } else if self.r#match(&[TokenType::Nil]) {
            Expr::Literal { value: Object::Nil }
        } else if self.r#match(&[TokenType::String, TokenType::Integer, TokenType::Float]) {
            Expr::Literal {
                value: token.literal.unwrap_or_default(),
            }
        } else if self.r#match(&[TokenType::Identifier]) {
            Expr::Variable {
                name: self.previous().clone(),
            }
        } else if self.r#match(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(&TokenType::RightParen, "Expected ')' after expression.")?;
            Expr::Grouping {
                expr: Box::new(expr),
            }
        } else if self.r#match(&[TokenType::Super]) {
            let keyword = self.advance().clone();
            self.consume(&TokenType::Dot, "Expect '.' after 'super'.")?;
            let method = self.consume(&TokenType::Identifier, "Expect superclass method name.")?;

            Expr::Super { keyword, method }
        } else {
            return Err(Error::Runtime {
                token,
                message: "Expected expression".to_string(),
            });
        };
        Ok(expr)
    }

    fn r#match(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&mut self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().r#type == *token_type
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().r#type == TokenType::Eof
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn consume(&mut self, token_type: &TokenType, message: &str) -> Result<Token, Error> {
        if self.check(token_type) {
            Ok(self.advance().clone())
        } else {
            Err(self.error(&self.peek(), message))
        }
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().r#type == TokenType::Semicolon {
                return;
            }

            match self.peek().r#type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => {}
            }

            self.advance();
        }
    }

    fn error(&self, token: &Token, message: &str) -> Error {
        parser_error(token, message);
        Error::Parse {
            token: token.clone(),
            message: message.to_string(),
        }
    }
}
