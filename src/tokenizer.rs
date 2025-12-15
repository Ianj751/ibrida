use std::{
    char,
    collections::{HashMap, binary_heap::PeekMut},
    iter::Peekable,
    vec::IntoIter,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    //Keywords
    Func,
    Return,

    //Types
    Integer, // will change this to more specific types: i32, u8, etc.

    Identifier(String), //name of a variable or function
    Literal(String),

    //Operators
    Addition,
    Subtraction,
    Division,
    Multiplication,
    Assignment,

    //Delimiters
    OpeningBrace,
    ClosingBrace,
    OpeningParenthesis,
    ClosingParenthesis,
    Colon, //used for denoting types of vars and return types
    Semicolon,

    Eof,
}

// Produces a Vector of tokens to be iterated through by the parser
pub struct Tokenizer {
    // content: String,
    input_iterator: Peekable<IntoIter<char>>,
    token_map: HashMap<String, Token>,
}
impl Tokenizer {
    pub fn new(value: String) -> Self {
        let map = HashMap::from([
            (String::from("func"), Token::Func),
            //  (String::from("let"), Token::KeywordLet),
            (String::from("return"), Token::Return),
            (String::from("+"), Token::Addition),
            (String::from("="), Token::Assignment),
            (String::from("{"), Token::OpeningBrace),
            (String::from("}"), Token::ClosingBrace),
            (String::from("("), Token::OpeningParenthesis),
            (String::from(")"), Token::ClosingParenthesis),
            (String::from(":"), Token::Colon),
            (String::from(";"), Token::Semicolon),
        ]);
        let value = value.trim().to_owned();
        Self {
            input_iterator: value.chars().collect::<Vec<_>>().into_iter().peekable(),
            // content: value,
            token_map: map,
        }
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

        while self
            .input_iterator
            .peek()
            .is_some_and(|&ch| !char::is_whitespace(ch))
        {
            //can unwrap here bc we've verified its Some based on the if condition
            lexeme.push(self.input_iterator.next().unwrap());
        }

        let lexeme: String = lexeme.iter().collect();
        if lexeme.is_empty() {
            return Token::Eof;
        }

        self.token_map
            .get(&lexeme)
            .cloned()
            .unwrap_or_else(|| Token::Identifier(lexeme))
    }

    pub fn get_token_array(mut self) -> Vec<Token> {
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
