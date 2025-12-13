use std::{cell::Cell, char, collections::HashMap, vec::IntoIter};

use crate::stream::Stream;

#[derive(Clone)]
//for the moment only focus on doing main.ibi:
// func main(): int { return 69 + 420; }
pub enum Token {
    KeywordFunc,
    //    KeywordLet,
    KeywordReturn,
    TypeInt,            // will change this to more specific types: i32, u8, etc.
    Identifier(String), //name of a variable or function
    //    Assignment,
    Literal(String),
    OpAddition,
    SepOpeningBrace, //{
    SepClosingBrace, //}
    SepOpeningParenthesis,
    SepClosingParenthesis,
    //    Colon, //used for denoting types of vars and return types
    Semicolon,
}

// Produces a stream of tokens to be utilized by the parser
pub struct TokenStream {
    content: String,
    input_iterator: IntoIter<char>,
    token_map: HashMap<String, Token>,
}
impl Stream<Token> for TokenStream {
    fn new(value: String) -> Self {
        let map = HashMap::from([
            (String::from("func"), Token::KeywordFunc),
            //  (String::from("let"), Token::KeywordLet),
            (String::from("return"), Token::KeywordReturn),
            //   (String::from("="), Token::Assignment),
            (String::from("{"), Token::SepOpeningBrace),
            (String::from("}"), Token::SepClosingBrace),
            (String::from("("), Token::SepOpeningParenthesis),
            (String::from(")"), Token::SepClosingParenthesis),
            //    (String::from(":"), Token::Colon),
        ]);
        let value = value.trim().to_owned();
        Self {
            input_iterator: value.chars().collect::<Vec<_>>().into_iter(),
            content: value,
            token_map: map,
        }
    }

    fn peek(&self) -> Option<Token> {
        todo!()
    }

    // read until next whitespace, return a single token
    fn next(&mut self) -> Option<Token> {
        let mut lexeme: Vec<char> = Vec::new();
        while let Some(ch) = self.input_iterator.next()
            && !char::is_whitespace(ch)
        {
            lexeme.push(ch);
        }

        let lexeme: String = lexeme.iter().collect();
        if lexeme.is_empty() {
            return None;
        }

        self.token_map
            .get(&lexeme)
            .cloned()
            .or_else(|| Some(Token::Identifier(lexeme)))
    }

    fn is_end(&self) -> bool {
        todo!()
    }
}
