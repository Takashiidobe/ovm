#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Number(i64),
    Plus,
    Minus,
}

pub struct Tokenizer;

impl Tokenizer {
    pub fn tokenize(input: &str) -> Result<Vec<Token>, String> {
        let input = input.trim();
        if input.is_empty() {
            return Err("Empty input".to_string());
        }

        let mut tokens = Vec::new();
        let mut current_num = String::new();

        // Process each character
        for c in input.chars() {
            match c {
                '0'..='9' => {
                    // Collect digits into the current number
                    current_num.push(c);
                }
                '+' | '-' | ' ' => {
                    // If we have collected digits, convert them to a token
                    if !current_num.is_empty() {
                        let num = current_num.parse::<i64>().map_err(|e| e.to_string())?;
                        tokens.push(Token::Number(num));
                        current_num.clear();
                    }
                    match c {
                        '+' => tokens.push(Token::Plus),
                        '-' => tokens.push(Token::Minus),
                        ' ' => continue,
                        _ => unreachable!(),
                    }
                }
                _ => return Err(format!("Invalid character: {}", c)),
            }
        }

        // Handle the last number if there is one
        if !current_num.is_empty() {
            let num = current_num.parse::<i64>().map_err(|e| e.to_string())?;
            tokens.push(Token::Number(num));
        }

        // Validate the token sequence
        if tokens.is_empty() {
            return Err("No valid tokens found".to_string());
        }

        // Check if expression starts with an operator
        match tokens.first() {
            Some(Token::Plus) | Some(Token::Minus) => {
                return Err("Expression cannot start with an operator".to_string());
            }
            _ => {}
        }

        // Check if expression ends with an operator
        match tokens.last() {
            Some(Token::Plus) | Some(Token::Minus) => {
                return Err("Expression cannot end with an operator".to_string());
            }
            _ => {}
        }

        // Check for consecutive operators
        for i in 0..tokens.len() - 1 {
            match (&tokens[i], &tokens[i + 1]) {
                (Token::Plus, Token::Plus)
                | (Token::Plus, Token::Minus)
                | (Token::Minus, Token::Plus)
                | (Token::Minus, Token::Minus) => {
                    return Err("Consecutive operators are not allowed".to_string());
                }
                _ => {}
            }
        }

        Ok(tokens)
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
}
