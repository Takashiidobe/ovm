use std::convert;
use std::fmt;
use std::io;

use crate::frontend::token::{Object, Token, TokenType};

pub fn error(line: usize, message: &str) {
    report(line, "", message);
}

pub fn report(line: usize, loc: &str, message: &str) {
    eprintln!("[line {}] Error{}: {}", line, loc, message);
}

pub fn parser_error(token: &Token, message: &str) {
    if token.r#type == TokenType::Eof {
        report(token.line, " at end", message);
    } else {
        report(token.line, &format!(" at '{}'", token.lexeme), message);
    }
}

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Parse { token: Token, message: String },
    Runtime { token: Token, message: String },
    Return { value: Object },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Io(underlying) => write!(f, "IoError {}", underlying),
            Error::Parse { token, message } => {
                write!(f, "ParseError at token: {}, message: {}", token, message)
            }
            Error::Runtime { token, message } => {
                write!(f, "RuntimeError at token: {}, message: {}", token, message)
            }
            Error::Return { value } => write!(f, "Return {:?}", value),
        }
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        "Lox Error"
    }
}

impl convert::From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::Io(e)
    }
}
