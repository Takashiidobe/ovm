use crate::frontend::tokenizer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Num(i64),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
}

pub struct Parser;

impl Parser {
    pub fn parse(tokens: &[Token]) -> Result<Expr, String> {
        if tokens.is_empty() {
            return Err("No tokens to parse".to_string());
        }

        Self::parse_expr(tokens, 0).map(|(expr, _)| expr)
    }

    // Parse an expression with operator precedence
    fn parse_expr(tokens: &[Token], pos: usize) -> Result<(Expr, usize), String> {
        // Parse the first term
        let (mut left, mut next_pos) = Self::parse_term(tokens, pos)?;

        // Process remaining terms with + and - operators
        while next_pos < tokens.len() {
            match tokens[next_pos] {
                Token::Plus => {
                    let (right, new_pos) = Self::parse_term(tokens, next_pos + 1)?;
                    left = Expr::Add(Box::new(left), Box::new(right));
                    next_pos = new_pos;
                }
                Token::Minus => {
                    let (right, new_pos) = Self::parse_term(tokens, next_pos + 1)?;
                    left = Expr::Sub(Box::new(left), Box::new(right));
                    next_pos = new_pos;
                }
                _ => break, // Not a + or - operator, end of expression
            }
        }

        Ok((left, next_pos))
    }

    // Parse a term (multiplication and division)
    fn parse_term(tokens: &[Token], pos: usize) -> Result<(Expr, usize), String> {
        // Parse the first factor
        let (mut left, mut next_pos) = Self::parse_factor(tokens, pos)?;

        // Process remaining factors with * and / operators
        while next_pos < tokens.len() {
            match tokens[next_pos] {
                Token::Multiply => {
                    let (right, new_pos) = Self::parse_factor(tokens, next_pos + 1)?;
                    left = Expr::Mul(Box::new(left), Box::new(right));
                    next_pos = new_pos;
                }
                Token::Divide => {
                    let (right, new_pos) = Self::parse_factor(tokens, next_pos + 1)?;
                    left = Expr::Div(Box::new(left), Box::new(right));
                    next_pos = new_pos;
                }
                _ => break, // Not a * or / operator, end of term
            }
        }

        Ok((left, next_pos))
    }

    // Parse a factor (numbers)
    fn parse_factor(tokens: &[Token], pos: usize) -> Result<(Expr, usize), String> {
        if pos >= tokens.len() {
            return Err("Unexpected end of expression".to_string());
        }

        match &tokens[pos] {
            Token::Number(n) => Ok((Expr::Num(*n), pos + 1)),
            _ => Err(format!("Expected number at position {}", pos)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::tokenizer::{Token, Tokenizer};

    #[test]
    fn test_parse_single_number() {
        let tokens = vec![Token::Number(123)];
        assert_eq!(Parser::parse(&tokens).unwrap(), Expr::Num(123));
    }

    #[test]
    fn test_parse_addition() {
        let tokens = vec![Token::Number(123), Token::Plus, Token::Number(456)];
        assert_eq!(
            Parser::parse(&tokens).unwrap(),
            Expr::Add(Box::new(Expr::Num(123)), Box::new(Expr::Num(456)))
        );
    }

    #[test]
    fn test_parse_subtraction() {
        let tokens = vec![Token::Number(123), Token::Minus, Token::Number(456)];
        assert_eq!(
            Parser::parse(&tokens).unwrap(),
            Expr::Sub(Box::new(Expr::Num(123)), Box::new(Expr::Num(456)))
        );
    }

    #[test]
    fn test_parse_multiplication() {
        let tokens = vec![Token::Number(123), Token::Multiply, Token::Number(456)];
        assert_eq!(
            Parser::parse(&tokens).unwrap(),
            Expr::Mul(Box::new(Expr::Num(123)), Box::new(Expr::Num(456)))
        );
    }

    #[test]
    fn test_parse_division() {
        let tokens = vec![Token::Number(123), Token::Divide, Token::Number(456)];
        assert_eq!(
            Parser::parse(&tokens).unwrap(),
            Expr::Div(Box::new(Expr::Num(123)), Box::new(Expr::Num(456)))
        );
    }

    #[test]
    fn test_parse_complex() {
        // 123 + 456 - 789
        // This should be parsed as ((123 + 456) - 789) due to left-to-right evaluation
        let tokens = vec![
            Token::Number(123),
            Token::Plus,
            Token::Number(456),
            Token::Minus,
            Token::Number(789),
        ];

        assert_eq!(
            Parser::parse(&tokens).unwrap(),
            Expr::Sub(
                Box::new(Expr::Add(
                    Box::new(Expr::Num(123)),
                    Box::new(Expr::Num(456))
                )),
                Box::new(Expr::Num(789))
            )
        );
    }

    #[test]
    fn test_parse_operator_precedence() {
        // 123 + 456 * 789
        // This should be parsed as (123 + (456 * 789)) due to operator precedence
        let tokens = vec![
            Token::Number(123),
            Token::Plus,
            Token::Number(456),
            Token::Multiply,
            Token::Number(789),
        ];

        assert_eq!(
            Parser::parse(&tokens).unwrap(),
            Expr::Add(
                Box::new(Expr::Num(123)),
                Box::new(Expr::Mul(
                    Box::new(Expr::Num(456)),
                    Box::new(Expr::Num(789))
                ))
            )
        );
    }

    #[test]
    fn test_parse_mixed_precedence() {
        // 10 * 20 + 30 / 5
        // This should be parsed as ((10 * 20) + (30 / 5))
        let tokens = vec![
            Token::Number(10),
            Token::Multiply,
            Token::Number(20),
            Token::Plus,
            Token::Number(30),
            Token::Divide,
            Token::Number(5),
        ];

        assert_eq!(
            Parser::parse(&tokens).unwrap(),
            Expr::Add(
                Box::new(Expr::Mul(Box::new(Expr::Num(10)), Box::new(Expr::Num(20)))),
                Box::new(Expr::Div(Box::new(Expr::Num(30)), Box::new(Expr::Num(5))))
            )
        );
    }

    #[test]
    fn test_parse_invalid_sequence() {
        let tokens = vec![Token::Number(123), Token::Plus, Token::Plus];
        assert!(Parser::parse(&tokens).is_err());
    }

    // Integration test that uses both tokenizer and parser
    #[test]
    fn test_tokenize_and_parse() {
        let input = "123 + 456 - 789";
        let tokens = Tokenizer::tokenize(input).unwrap();
        let expr = Parser::parse(&tokens).unwrap();

        assert_eq!(
            expr,
            Expr::Sub(
                Box::new(Expr::Add(
                    Box::new(Expr::Num(123)),
                    Box::new(Expr::Num(456))
                )),
                Box::new(Expr::Num(789))
            )
        );
    }

    #[test]
    fn test_tokenize_and_parse_with_precedence() {
        let input = "2 + 3 * 4";
        let tokens = Tokenizer::tokenize(input).unwrap();
        let expr = Parser::parse(&tokens).unwrap();

        assert_eq!(
            expr,
            Expr::Add(
                Box::new(Expr::Num(2)),
                Box::new(Expr::Mul(Box::new(Expr::Num(3)), Box::new(Expr::Num(4))))
            )
        );
    }
}
