use crate::frontend::tokenizer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Num(i64),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Print(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Expr>,
}

pub struct Parser;

impl Parser {
    pub fn parse(tokens: &[Token]) -> Result<Program, String> {
        if tokens.is_empty() {
            return Err("No tokens to parse".to_string());
        }

        let mut statements = Vec::new();
        let mut position = 0;

        while position < tokens.len() {
            let (expr, next_pos) = Self::parse_expr(tokens, position)?;
            statements.push(expr);
            
            // If we reached the end of the tokens, we're done
            if next_pos >= tokens.len() {
                break;
            }
            
            // Otherwise, we should have a semicolon
            match tokens[next_pos] {
                Token::Semicolon => {
                    position = next_pos + 1; // Move past the semicolon
                }
                _ => {
                    // If we didn't reach the end and don't have a semicolon, it's an error
                    return Err(format!("Expected semicolon at position {}", next_pos));
                }
            }
        }
        
        if statements.is_empty() {
            return Err("No valid statements found".to_string());
        }
        
        Ok(Program { statements })
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

    // Parse a factor (numbers or parenthesized expressions)
    fn parse_factor(tokens: &[Token], pos: usize) -> Result<(Expr, usize), String> {
        if pos >= tokens.len() {
            return Err("Unexpected end of expression".to_string());
        }

        match &tokens[pos] {
            Token::Print => {
                // We expect the next token to be an open parenthesis
                if pos + 1 >= tokens.len() {
                    return Err("Expected expression after 'print'".to_string());
                }
                
                // Parse the expression inside print()
                let (expr, next_pos) = Self::parse_expr(tokens, pos + 1)?;
                
                Ok((Expr::Print(Box::new(expr)), next_pos))
            },
            Token::Number(n) => Ok((Expr::Num(*n), pos + 1)),
            Token::LeftParen => {
                // Parse a parenthesized expression
                let (expr, next_pos) = Self::parse_expr(tokens, pos + 1)?;
                
                // Ensure there's a matching right parenthesis
                if next_pos >= tokens.len() {
                    return Err("Missing closing parenthesis".to_string());
                }
                match &tokens[next_pos] {
                    Token::RightParen => Ok((expr, next_pos + 1)),
                    _ => Err("Expected closing parenthesis".to_string()),
                }
            },
            _ => Err(format!("Expected number or opening parenthesis at position {}", pos)),
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
        let program = Parser::parse(&tokens).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert_eq!(program.statements[0], Expr::Num(123));
    }

    #[test]
    fn test_parse_addition() {
        let tokens = vec![Token::Number(123), Token::Plus, Token::Number(456)];
        let program = Parser::parse(&tokens).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
            Expr::Add(Box::new(Expr::Num(123)), Box::new(Expr::Num(456)))
        );
    }

    #[test]
    fn test_parse_subtraction() {
        let tokens = vec![Token::Number(123), Token::Minus, Token::Number(456)];
        let program = Parser::parse(&tokens).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
            Expr::Sub(Box::new(Expr::Num(123)), Box::new(Expr::Num(456)))
        );
    }

    #[test]
    fn test_parse_multiplication() {
        let tokens = vec![Token::Number(123), Token::Multiply, Token::Number(456)];
        let program = Parser::parse(&tokens).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
            Expr::Mul(Box::new(Expr::Num(123)), Box::new(Expr::Num(456)))
        );
    }

    #[test]
    fn test_parse_division() {
        let tokens = vec![Token::Number(123), Token::Divide, Token::Number(456)];
        let program = Parser::parse(&tokens).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
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

        let program = Parser::parse(&tokens).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
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

        let program = Parser::parse(&tokens).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
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

        let program = Parser::parse(&tokens).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
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
        let program = Parser::parse(&tokens).unwrap();

        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
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
        let program = Parser::parse(&tokens).unwrap();

        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
            Expr::Add(
                Box::new(Expr::Num(2)),
                Box::new(Expr::Mul(Box::new(Expr::Num(3)), Box::new(Expr::Num(4))))
            )
        );
    }
    
    #[test]
    fn test_parse_parentheses() {
        let input = "(2 + 3) * 4";
        let tokens = Tokenizer::tokenize(input).unwrap();
        let program = Parser::parse(&tokens).unwrap();
        
        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
            Expr::Mul(
                Box::new(Expr::Add(Box::new(Expr::Num(2)), Box::new(Expr::Num(3)))),
                Box::new(Expr::Num(4))
            )
        );
    }
    
    #[test]
    fn test_parse_nested_parentheses() {
        let input = "2 * (3 + (4 - 1))";
        let tokens = Tokenizer::tokenize(input).unwrap();
        let program = Parser::parse(&tokens).unwrap();
        
        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
            Expr::Mul(
                Box::new(Expr::Num(2)),
                Box::new(Expr::Add(
                    Box::new(Expr::Num(3)),
                    Box::new(Expr::Sub(Box::new(Expr::Num(4)), Box::new(Expr::Num(1))))
                ))
            )
        );
    }
    
    #[test]
    fn test_multiple_statements() {
        let input = "1; 2; 3";
        let tokens = Tokenizer::tokenize(input).unwrap();
        let program = Parser::parse(&tokens).unwrap();
        
        assert_eq!(program.statements.len(), 3);
        assert_eq!(program.statements[0], Expr::Num(1));
        assert_eq!(program.statements[1], Expr::Num(2));
        assert_eq!(program.statements[2], Expr::Num(3));
    }
    
    #[test]
    fn test_complex_multiple_statements() {
        let input = "1 + 2; 3 * 4; 5 - 6";
        let tokens = Tokenizer::tokenize(input).unwrap();
        let program = Parser::parse(&tokens).unwrap();
        
        assert_eq!(program.statements.len(), 3);
        assert_eq!(
            program.statements[0], 
            Expr::Add(Box::new(Expr::Num(1)), Box::new(Expr::Num(2)))
        );
        assert_eq!(
            program.statements[1], 
            Expr::Mul(Box::new(Expr::Num(3)), Box::new(Expr::Num(4)))
        );
        assert_eq!(
            program.statements[2], 
            Expr::Sub(Box::new(Expr::Num(5)), Box::new(Expr::Num(6)))
        );
    }
    
    #[test]
    fn test_statements_with_parentheses() {
        let input = "(1 + 2) * 3; 4 + (5 * 6)";
        let tokens = Tokenizer::tokenize(input).unwrap();
        let program = Parser::parse(&tokens).unwrap();
        
        assert_eq!(program.statements.len(), 2);
        assert_eq!(
            program.statements[0], 
            Expr::Mul(
                Box::new(Expr::Add(Box::new(Expr::Num(1)), Box::new(Expr::Num(2)))),
                Box::new(Expr::Num(3))
            )
        );
        assert_eq!(
            program.statements[1], 
            Expr::Add(
                Box::new(Expr::Num(4)),
                Box::new(Expr::Mul(Box::new(Expr::Num(5)), Box::new(Expr::Num(6))))
            )
        );
    }
    
    #[test]
    fn test_parse_print() {
        let input = "print(123)";
        let tokens = Tokenizer::tokenize(input).unwrap();
        let program = Parser::parse(&tokens).unwrap();
        
        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
            Expr::Print(Box::new(Expr::Num(123)))
        );
    }
    
    #[test]
    fn test_parse_complex_print() {
        let input = "print(10 + 20 * 30)";
        let tokens = Tokenizer::tokenize(input).unwrap();
        let program = Parser::parse(&tokens).unwrap();
        
        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
            Expr::Print(
                Box::new(Expr::Add(
                    Box::new(Expr::Num(10)),
                    Box::new(Expr::Mul(
                        Box::new(Expr::Num(20)),
                        Box::new(Expr::Num(30))
                    ))
                ))
            )
        );
    }
    
    #[test]
    fn test_multiple_prints() {
        let input = "print(1); print(2 + 3)";
        let tokens = Tokenizer::tokenize(input).unwrap();
        let program = Parser::parse(&tokens).unwrap();
        
        assert_eq!(program.statements.len(), 2);
        assert_eq!(
            program.statements[0],
            Expr::Print(Box::new(Expr::Num(1)))
        );
        assert_eq!(
            program.statements[1],
            Expr::Print(
                Box::new(Expr::Add(
                    Box::new(Expr::Num(2)),
                    Box::new(Expr::Num(3))
                ))
            )
        );
    }
}
