use crate::frontend::tokenizer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Num(i64),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
}

pub struct Parser;

impl Parser {
    pub fn parse(tokens: &[Token]) -> Result<Expr, String> {
        if tokens.is_empty() {
            return Err("No tokens to parse".to_string());
        }

        // Initialize with the first number
        let first_token = &tokens[0];
        match first_token {
            Token::Number(n) => {
                let mut result = Expr::Num(*n);

                // Process the rest of the tokens in pairs (operator, number)
                let mut i = 1;
                while i < tokens.len() {
                    let op_token = &tokens[i];

                    // Ensure we have a number after the operator
                    if i + 1 >= tokens.len() {
                        return Err("Expected number after operator".to_string());
                    }

                    let num_token = &tokens[i + 1];
                    match (op_token, num_token) {
                        (Token::Plus, Token::Number(n)) => {
                            result = Expr::Add(Box::new(result), Box::new(Expr::Num(*n)));
                        }
                        (Token::Minus, Token::Number(n)) => {
                            result = Expr::Sub(Box::new(result), Box::new(Expr::Num(*n)));
                        }
                        _ => {
                            return Err(format!("Invalid token sequence at position {}", i));
                        }
                    }

                    // Move to the next operator
                    i += 2;
                }

                Ok(result)
            }
            _ => Err("Expression must start with a number".to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::{Token, Tokenizer};

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
}
