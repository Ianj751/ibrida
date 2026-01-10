use std::{char, collections::HashMap, iter::Peekable, vec::IntoIter};

use regex::Regex;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    //Keywords
    Func,
    Return,
    Let,

    //Types
    Integer32, // will change this to more specific types: i32, u8, etc.
    Float32,
    String,

    Identifier(String), //name of a variable or function
    //NumberLiteral(String),
    FloatLiteral(String),
    IntegerLiteral(String),

    //Operators
    Op(Operator),

    //Delimiters
    OpenBrace,
    CloseBrace,
    OpenParenthesis,
    CloseParenthesis,
    Colon, //used for denoting types of vars and return types
    Semicolon,
    Comma,

    Eof,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Operator {
    Addition,
    Subtraction,
    Division,
    Multiplication,
    Assignment,
}

// Produces a Vector of tokens to be iterated through by the parser
pub struct Tokenizer {
    input_iterator: Peekable<IntoIter<char>>,
    token_map: HashMap<String, Token>,
}
impl Tokenizer {
    pub fn new(value: String) -> Self {
        let map = HashMap::from([
            (String::from("func"), Token::Func),
            (String::from("let"), Token::Let),
            (String::from("return"), Token::Return),
            (String::from("+"), Token::Op(Operator::Addition)),
            (String::from("-"), Token::Op(Operator::Subtraction)),
            (String::from("/"), Token::Op(Operator::Division)),
            (String::from("*"), Token::Op(Operator::Multiplication)),
            (String::from("="), Token::Op(Operator::Assignment)),
            (String::from("i32"), Token::Integer32),
            (String::from("f32"), Token::Float32),
            (String::from("string"), Token::String),
        ]);
        let value = value.trim().to_owned();
        Self {
            input_iterator: value.chars().collect::<Vec<_>>().into_iter().peekable(),
            token_map: map,
        }
    }
    fn is_delimiter(ch: char) -> bool {
        matches!(ch, '{' | '}' | '(' | ')' | ':' | ';' | ',')
    }

    // read until next whitespace, return a single token
    fn get_next_token(&mut self) -> Token {
        let mut lexeme: Vec<char> = Vec::new();

        // Skip whitespace
        while self
            .input_iterator
            .peek()
            .is_some_and(|&ch| char::is_whitespace(ch))
        {
            self.input_iterator.next();
        }
        // should modify this logic for delimiters
        if let Some(&ch) = self.input_iterator.peek()
            && Self::is_delimiter(ch)
        {
            self.input_iterator.next();
            return match ch {
                '{' => Token::OpenBrace,
                '}' => Token::CloseBrace,
                '(' => Token::OpenParenthesis,
                ')' => Token::CloseParenthesis,
                ':' => Token::Colon,
                ';' => Token::Semicolon,
                ',' => Token::Comma,
                _ => unreachable!(),
            };
        }

        while let Some(&ch) = self.input_iterator.peek() {
            if char::is_whitespace(ch) || Self::is_delimiter(ch) {
                break;
            }

            lexeme.push(self.input_iterator.next().unwrap());
        }

        let lexeme: String = lexeme.iter().collect();
        if lexeme.is_empty() {
            return Token::Eof;
        }

        let float_regex =
            Regex::new(r"^[+-]?(?:\d+\.\d+|\d+\.|\.\d+)$").expect("invalid float regex supplied");
        let int_regex = Regex::new(r"^[+-]?\d+$").expect("invalid int regex supplied");

        if int_regex.is_match(lexeme.as_str()) {
            return Token::IntegerLiteral(lexeme);
        } else if float_regex.is_match(lexeme.as_str()) {
            return Token::FloatLiteral(lexeme);
        }

        self.token_map
            .get(&lexeme)
            .cloned()
            .unwrap_or(Token::Identifier(lexeme))
    }

    pub fn get_tokens(mut self) -> Vec<Token> {
        let mut v = Vec::new();
        while let tok = self.get_next_token()
            && tok != Token::Eof
        {
            v.push(tok);
        }
        v.push(Token::Eof);
        v
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_get_tokens_keywords() {
        let file_contents = String::from("  let\t\nfunc return  ");
        let expected = vec![Token::Let, Token::Func, Token::Return, Token::Eof];
        let got = Tokenizer::new(file_contents).get_tokens();

        assert_eq!(expected, got);
    }
    #[test]
    fn test_get_tokens_operators() {
        let file_contents = String::from("  \t\n+\n\t  *  \t\n\n/\t  \n-\n\t  =\t\n  ");
        let expected = vec![
            Token::Op(Operator::Addition),
            Token::Op(Operator::Multiplication),
            Token::Op(Operator::Division),
            Token::Op(Operator::Subtraction),
            Token::Op(Operator::Assignment),
            Token::Eof,
        ];
        let got = Tokenizer::new(file_contents).get_tokens();

        assert_eq!(expected, got);
    }
    #[test]
    fn test_get_tokens_delimiters() {
        let file_contents = String::from("  {\t\n  (\t:\n\n;\t  )\n  }  \t{\n(\t\n:\t;\n\n ");
        let expected = vec![
            Token::OpenBrace,
            Token::OpenParenthesis,
            Token::Colon,
            Token::Semicolon,
            Token::CloseParenthesis,
            Token::CloseBrace,
            Token::OpenBrace,
            Token::OpenParenthesis,
            Token::Colon,
            Token::Semicolon,
            Token::Eof,
        ];
        let got = Tokenizer::new(file_contents).get_tokens();

        assert_eq!(expected, got);
    }

    #[test]
    fn test_get_tokens_main() {
        let file_contents = "func main(): i32 { return 69 + 4.20;}".to_string();
        let expected = vec![
            Token::Func,
            Token::Identifier("main".to_string()),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::Colon,
            Token::Integer32,
            Token::OpenBrace,
            Token::Return,
            Token::IntegerLiteral("69".to_string()),
            Token::Op(Operator::Addition),
            Token::FloatLiteral("4.20".to_string()),
            Token::Semicolon,
            Token::CloseBrace,
            Token::Eof,
        ];
        let got = Tokenizer::new(file_contents).get_tokens();
        assert_eq!(expected, got);
    }
}
