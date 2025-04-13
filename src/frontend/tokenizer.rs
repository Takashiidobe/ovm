#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Number(i64),
    Plus,
    Minus,
    Multiply,
    Divide,
    LeftParen,
    RightParen,
    Semicolon,
    Print,
}

pub struct Tokenizer;

impl Tokenizer {
    pub fn tokenize(input: &str) -> Result<Vec<Token>, String> {
        let input = input.trim();
        if input.is_empty() {
            return Err("Empty input".to_string());
        }

        let mut position = 0;
        let mut tokens = Vec::new();

        // Process the input until we've consumed it all
        while position < input.len() {
            position = Self::next_token(input, position, &mut tokens)?;
        }

        // Validate the resulting token sequence
        Self::validate_tokens(&tokens)?;

        Ok(tokens)
    }

    // Try to extract the next token from the input starting at position
    fn next_token(
        input: &str,
        mut position: usize,
        tokens: &mut Vec<Token>,
    ) -> Result<usize, String> {
        // Skip whitespace
        while position < input.len() && input[position..].chars().next().unwrap().is_whitespace() {
            position += 1;
        }

        if position >= input.len() {
            return Ok(position);
        }

        let next_char = input[position..].chars().next().unwrap();

        // Check for keywords
        if next_char.is_alphabetic() {
            // Check for 'print' keyword
            if position + 5 <= input.len() && &input[position..position + 5] == "print" {
                // Make sure it's followed by a left parenthesis (maybe with whitespace)
                let mut paren_pos = position + 5;
                while paren_pos < input.len()
                    && input[paren_pos..].chars().next().unwrap().is_whitespace()
                {
                    paren_pos += 1;
                }

                if paren_pos < input.len() && input[paren_pos..].chars().next().unwrap() == '(' {
                    tokens.push(Token::Print);
                    return Ok(position + 5);
                }
            }

            // If we reach here, it's an unknown identifier
            return Err(format!("Unknown identifier at position {}", position));
        }

        // Handle numbers
        if next_char.is_ascii_digit() {
            let mut end_pos = position + 1;
            while end_pos < input.len() && input[end_pos..].chars().next().unwrap().is_ascii_digit()
            {
                end_pos += 1;
            }

            let num_str = &input[position..end_pos];
            let num = num_str.parse::<i64>().map_err(|e| e.to_string())?;
            tokens.push(Token::Number(num));
            return Ok(end_pos);
        }

        // Handle operators and other symbols
        match next_char {
            '+' => {
                tokens.push(Token::Plus);
                return Ok(position + 1);
            }
            '-' => {
                tokens.push(Token::Minus);
                return Ok(position + 1);
            }
            '*' => {
                tokens.push(Token::Multiply);
                return Ok(position + 1);
            }
            '/' => {
                tokens.push(Token::Divide);
                return Ok(position + 1);
            }
            '(' => {
                tokens.push(Token::LeftParen);
                return Ok(position + 1);
            }
            ')' => {
                tokens.push(Token::RightParen);
                return Ok(position + 1);
            }
            ';' => {
                tokens.push(Token::Semicolon);
                return Ok(position + 1);
            }
            _ => return Err(format!("Invalid character: {}", next_char)),
        }
    }

    // Validate the sequence of tokens
    fn validate_tokens(tokens: &[Token]) -> Result<(), String> {
        if tokens.is_empty() {
            return Err("No valid tokens found".to_string());
        }

        // Check if expression starts with an operator (but opening parenthesis and print are allowed)
        match tokens.first() {
            Some(Token::Plus)
            | Some(Token::Minus)
            | Some(Token::Multiply)
            | Some(Token::Divide)
            | Some(Token::Semicolon) => {
                return Err("Expression cannot start with an operator or semicolon".to_string());
            }
            _ => {}
        }

        // Check if expression ends with an operator (but closing parenthesis is allowed)
        match tokens.last() {
            Some(Token::Plus)
            | Some(Token::Minus)
            | Some(Token::Multiply)
            | Some(Token::Divide) => {
                return Err("Expression cannot end with an operator".to_string());
            }
            _ => {}
        }

        // Check for balanced parentheses
        let mut paren_count = 0;
        for token in tokens.iter() {
            match token {
                Token::LeftParen => paren_count += 1,
                Token::RightParen => {
                    paren_count -= 1;
                    if paren_count < 0 {
                        return Err("Unbalanced parentheses".to_string());
                    }
                }
                _ => {}
            }
        }
        if paren_count != 0 {
            return Err("Unbalanced parentheses".to_string());
        }

        // Check for consecutive operators
        for i in 0..tokens.len() - 1 {
            match (&tokens[i], &tokens[i + 1]) {
                // Operators followed by a right parenthesis is allowed
                (_, Token::RightParen) => {}
                // Left parenthesis followed by an operator is allowed
                (Token::LeftParen, _) => {}
                // Semicolon followed by a number, left parenthesis, or print is allowed
                (Token::Semicolon, Token::Number(_))
                | (Token::Semicolon, Token::LeftParen)
                | (Token::Semicolon, Token::Print) => {}
                // Operator followed by semicolon is not allowed
                (Token::Plus, Token::Semicolon)
                | (Token::Minus, Token::Semicolon)
                | (Token::Multiply, Token::Semicolon)
                | (Token::Divide, Token::Semicolon) => {
                    return Err("Expression cannot end with an operator".to_string());
                }
                // Semicolon followed by operators is not allowed
                (Token::Semicolon, Token::Plus)
                | (Token::Semicolon, Token::Minus)
                | (Token::Semicolon, Token::Multiply)
                | (Token::Semicolon, Token::Divide) => {
                    return Err("Expression cannot start with an operator".to_string());
                }
                // Check for consecutive operators
                (Token::Plus, Token::Plus)
                | (Token::Plus, Token::Minus)
                | (Token::Plus, Token::Multiply)
                | (Token::Plus, Token::Divide)
                | (Token::Minus, Token::Plus)
                | (Token::Minus, Token::Minus)
                | (Token::Minus, Token::Multiply)
                | (Token::Minus, Token::Divide)
                | (Token::Multiply, Token::Plus)
                | (Token::Multiply, Token::Minus)
                | (Token::Multiply, Token::Multiply)
                | (Token::Multiply, Token::Divide)
                | (Token::Divide, Token::Plus)
                | (Token::Divide, Token::Minus)
                | (Token::Divide, Token::Multiply)
                | (Token::Divide, Token::Divide) => {
                    return Err("Consecutive operators are not allowed".to_string());
                }
                // Consecutive semicolons are not allowed (empty statements)
                (Token::Semicolon, Token::Semicolon) => {
                    return Err("Empty statements are not allowed".to_string());
                }
                _ => {}
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_simple() {
        assert_eq!(
            Tokenizer::tokenize("123").unwrap(),
            vec![Token::Number(123)]
        );
    }

    #[test]
    fn test_tokenize_parentheses() {
        assert_eq!(
            Tokenizer::tokenize("(123)").unwrap(),
            vec![Token::LeftParen, Token::Number(123), Token::RightParen]
        );
    }

    #[test]
    fn test_tokenize_addition() {
        assert_eq!(
            Tokenizer::tokenize("123 + 456").unwrap(),
            vec![Token::Number(123), Token::Plus, Token::Number(456)]
        );
    }

    #[test]
    fn test_tokenize_subtraction() {
        assert_eq!(
            Tokenizer::tokenize("123 - 456").unwrap(),
            vec![Token::Number(123), Token::Minus, Token::Number(456)]
        );
    }

    #[test]
    fn test_tokenize_complex() {
        assert_eq!(
            Tokenizer::tokenize("123 + 456 - 789").unwrap(),
            vec![
                Token::Number(123),
                Token::Plus,
                Token::Number(456),
                Token::Minus,
                Token::Number(789)
            ]
        );
    }

    #[test]
    fn test_tokenize_multiplication() {
        assert_eq!(
            Tokenizer::tokenize("123 * 456").unwrap(),
            vec![Token::Number(123), Token::Multiply, Token::Number(456)]
        );
    }

    #[test]
    fn test_tokenize_division() {
        assert_eq!(
            Tokenizer::tokenize("123 / 456").unwrap(),
            vec![Token::Number(123), Token::Divide, Token::Number(456)]
        );
    }

    #[test]
    fn test_tokenize_mixed_operators() {
        assert_eq!(
            Tokenizer::tokenize("123 + 456 * 789").unwrap(),
            vec![
                Token::Number(123),
                Token::Plus,
                Token::Number(456),
                Token::Multiply,
                Token::Number(789)
            ]
        );
    }

    #[test]
    fn test_tokenize_no_spaces() {
        assert_eq!(
            Tokenizer::tokenize("123+456-789").unwrap(),
            vec![
                Token::Number(123),
                Token::Plus,
                Token::Number(456),
                Token::Minus,
                Token::Number(789)
            ]
        );
    }

    #[test]
    fn test_tokenize_invalid_start() {
        assert!(Tokenizer::tokenize("+ 123").is_err());
    }

    #[test]
    fn test_tokenize_invalid_end() {
        assert!(Tokenizer::tokenize("123 +").is_err());
    }

    #[test]
    fn test_tokenize_consecutive_operators() {
        assert!(Tokenizer::tokenize("123 + - 456").is_err());
    }

    #[test]
    fn test_tokenize_nested_parentheses() {
        assert_eq!(
            Tokenizer::tokenize("((123 + 456) * 789)").unwrap(),
            vec![
                Token::LeftParen,
                Token::LeftParen,
                Token::Number(123),
                Token::Plus,
                Token::Number(456),
                Token::RightParen,
                Token::Multiply,
                Token::Number(789),
                Token::RightParen
            ]
        );
    }

    #[test]
    fn test_tokenize_unbalanced_parentheses() {
        assert!(Tokenizer::tokenize("(123 + 456").is_err());
        assert!(Tokenizer::tokenize("123 + 456)").is_err());
    }

    #[test]
    fn test_tokenize_semicolon() {
        assert_eq!(
            Tokenizer::tokenize("123;").unwrap(),
            vec![Token::Number(123), Token::Semicolon]
        );
    }

    #[test]
    fn test_tokenize_multiple_statements() {
        assert_eq!(
            Tokenizer::tokenize("1; 2; 3").unwrap(),
            vec![
                Token::Number(1),
                Token::Semicolon,
                Token::Number(2),
                Token::Semicolon,
                Token::Number(3)
            ]
        );
    }

    #[test]
    fn test_tokenize_complex_statements() {
        assert_eq!(
            Tokenizer::tokenize("1 + 2; 3 * 4").unwrap(),
            vec![
                Token::Number(1),
                Token::Plus,
                Token::Number(2),
                Token::Semicolon,
                Token::Number(3),
                Token::Multiply,
                Token::Number(4)
            ]
        );
    }

    #[test]
    fn test_tokenize_invalid_semicolon() {
        // Empty statements not allowed
        assert!(Tokenizer::tokenize("1;;3").is_err());
        // Can't start with semicolon
        assert!(Tokenizer::tokenize(";1").is_err());
    }

    #[test]
    fn test_tokenize_print() {
        assert_eq!(
            Tokenizer::tokenize("print(123)").unwrap(),
            vec![
                Token::Print,
                Token::LeftParen,
                Token::Number(123),
                Token::RightParen
            ]
        );
    }

    #[test]
    fn test_tokenize_complex_print() {
        assert_eq!(
            Tokenizer::tokenize("print(10 + 20 * 30)").unwrap(),
            vec![
                Token::Print,
                Token::LeftParen,
                Token::Number(10),
                Token::Plus,
                Token::Number(20),
                Token::Multiply,
                Token::Number(30),
                Token::RightParen
            ]
        );
    }
}

