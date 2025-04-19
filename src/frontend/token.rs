use std::{
    fmt,
    hash::{Hash, Hasher},
};

#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenType {
    // Single character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens
    BitOr,
    BitAnd,
    LeftShift,
    RightShift,
    Bang,
    BangEqual,
    Equal,
    Modulo,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals
    Identifier,
    String,
    Integer,
    Float,
    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    #[default]
    Eof,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub r#type: TokenType,
    pub lexeme: String,
    pub literal: Option<Object>,
    pub line: usize,
}

impl Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.lexeme.hash(state);
        self.line.hash(state);
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let val = match (&self.r#type, &self.literal) {
            (TokenType::LeftParen, _) => "(".to_string(),
            (TokenType::RightParen, _) => ")".to_string(),
            (TokenType::LeftBrace, _) => "{".to_string(),
            (TokenType::RightBrace, _) => "}".to_string(),
            (TokenType::Comma, _) => ",".to_string(),
            (TokenType::Dot, _) => ".".to_string(),
            (TokenType::Minus, _) => "-".to_string(),
            (TokenType::Plus, _) => "+".to_string(),
            (TokenType::Semicolon, _) => ";".to_string(),
            (TokenType::Slash, _) => "/".to_string(),
            (TokenType::Star, _) => "*".to_string(),
            (TokenType::Bang, _) => "!".to_string(),
            (TokenType::BangEqual, _) => "!=".to_string(),
            (TokenType::Equal, _) => "=".to_string(),
            (TokenType::EqualEqual, _) => "==".to_string(),
            (TokenType::Greater, _) => ">".to_string(),
            (TokenType::GreaterEqual, _) => ">=".to_string(),
            (TokenType::Less, _) => "<".to_string(),
            (TokenType::LessEqual, _) => "<=".to_string(),
            (TokenType::Identifier, Some(val))
            | (TokenType::String, Some(val))
            | (TokenType::Float, Some(val))
            | (TokenType::Integer, Some(val)) => val.to_string(),
            (TokenType::And, _) => "and".to_string(),
            (TokenType::Class, _) => "class".to_string(),
            (TokenType::Else, _) => "else".to_string(),
            (TokenType::False, _) => "false".to_string(),
            (TokenType::Fun, _) => "fun".to_string(),
            (TokenType::For, _) => "for".to_string(),
            (TokenType::If, _) => "if".to_string(),
            (TokenType::Nil, _) => "nil".to_string(),
            (TokenType::Or, _) => "or".to_string(),
            (TokenType::Print, _) => "print".to_string(),
            (TokenType::Return, _) => "return".to_string(),
            (TokenType::Super, _) => "super".to_string(),
            (TokenType::This, _) => "this".to_string(),
            (TokenType::True, _) => "true".to_string(),
            (TokenType::Var, _) => "var".to_string(),
            (TokenType::While, _) => "while".to_string(),
            (TokenType::Eof, _) => "eof".to_string(),
            (TokenType::Identifier, None)
            | (TokenType::String, None)
            | (TokenType::Integer, None)
            | (TokenType::Float, None) => panic!("Invalid token"),
            (TokenType::BitOr, _) => "|".to_string(),
            (TokenType::BitAnd, _) => "&".to_string(),
            (TokenType::LeftShift, _) => "<<".to_string(),
            (TokenType::RightShift, _) => ">>".to_string(),
            (TokenType::Modulo, _) => "%".to_string(),
        };

        f.write_str(&val)
    }
}

#[non_exhaustive]
#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub enum Object {
    String(String),
    Integer(i64),
    Float(f64),
    Identifier(String),
    Bool(bool),
    #[default]
    Nil,
}

impl Eq for Object {}

impl Object {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Object::Bool(false) | Object::Nil)
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::String(str) => f.write_str(str),
            Object::Integer(num) => f.write_str(&num.to_string()),
            Object::Float(num) => f.write_str(&num.to_string()),
            Object::Identifier(ident) => f.write_str(ident),
            Object::Bool(b) => f.write_str(&b.to_string()),
            Object::Nil => f.write_str("nil"),
        }
    }
}
