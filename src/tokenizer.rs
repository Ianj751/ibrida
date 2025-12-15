use std::{char, collections::HashMap, vec::IntoIter};

#[derive(Clone)]

pub enum Token {
    Keyw(Keyword),
    Type(ItemType),     // will change this to more specific types: i32, u8, etc.
    Identifier(String), //name of a variable or function
    Literal(String),
    Op(Operator),
    Delim(Delimiter),
}
#[derive(Clone)]
pub enum Keyword {
    Func,
    Return,
}
#[derive(Clone)]
pub enum Delimiter {
    OpeningBrace,
    ClosingBrace,
    OpeningParenthesis,
    ClosingParenthesis,
    Colon, //used for denoting types of vars and return types
    Semicolon,
}
#[derive(Clone)]
pub enum Operator {
    Addition,
    Subtraction,
    Division,
    Multiplication,
    // Assignment,
}
#[derive(Clone)]
pub enum ItemType {
    Integer,
}
// Produces a Vector of tokens to be iterated through by the parser
pub struct TokenStream {
    content: String,
    input_iterator: IntoIter<char>,
    token_map: HashMap<String, Token>,
    pub tokens: Vec<Token>,
}
impl TokenStream {
    fn new(value: String) -> Self {
        let map = HashMap::from([
            (String::from("func"), Token::Keyw(Keyword::Func)),
            //  (String::from("let"), Token::KeywordLet),
            (String::from("return"), Token::Keyw(Keyword::Return)),
            //   (String::from("="), Token::Assignment),
            (String::from("{"), Token::Delim(Delimiter::OpeningBrace)),
            (String::from("}"), Token::Delim(Delimiter::ClosingBrace)),
            (
                String::from("("),
                Token::Delim(Delimiter::OpeningParenthesis),
            ),
            (
                String::from(")"),
                Token::Delim(Delimiter::ClosingParenthesis),
            ),
            (String::from(":"), Token::Delim(Delimiter::Colon)),
            (String::from(";"), Token::Delim(Delimiter::Semicolon)),
        ]);
        let value = value.trim().to_owned();
        Self {
            input_iterator: value.chars().collect::<Vec<_>>().into_iter(),
            content: value,
            token_map: map,
            tokens: Vec::new(),
        }
    }

    // read until next whitespace, return a single token
    fn get_next_token(&mut self) -> Option<Token> {
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
}
