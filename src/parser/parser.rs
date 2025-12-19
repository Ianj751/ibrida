use std::{io::Error, iter::Peekable, vec::IntoIter};
use thiserror::Error;

use crate::{
    parser::ast::{BlockStmt, Expression, FuncDecl, LetStmt, ReturnStmt, Stmt},
    tokenizer::{Token, tokenizer::Operator},
};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("invalid token: expected {expected:?} but found {found:?}")]
    InvalidToken { expected: Token, found: Token },
    #[error("invalid token: expected an operator, but found `{0:?}`")]
    ExpectedOperator(Token),
    #[error("unexpected end of token stream")]
    UnexpectedEnd,
}
//https://craftinginterpreters.com/parsing-expressions.html
pub struct Parser {
    tokens: Vec<Token>,
}
fn get_op_binding_power(op: &Operator) -> (u8, u8) {
    match op {
        Operator::Addition | Operator::Subtraction => (1, 2),
        Operator::Division | Operator::Multiplication => (3, 4),
        Operator::Assignment => (0, 0),
    }
}

fn parse_arithmetic_expr(
    iter: &mut Peekable<IntoIter<Token>>,
    min_bp: u8,
) -> Result<Expression, ParseError> {
    let mut lhs = match iter.next() {
        Some(Token::NumberLiteral(x)) => Expression::UnaryExpr(x),
        t => {
            return Err(ParseError::InvalidToken {
                expected: Token::NumberLiteral("number".to_string()),
                found: t.unwrap_or(Token::Eof),
            });
        }
    };
    loop {
        let op = match iter.peek() {
            Some(Token::Eof | Token::Semicolon) => break,
            Some(Token::Op(op)) => op.clone(),
            t => {
                return Err(ParseError::ExpectedOperator(
                    t.unwrap_or(&Token::Eof).clone(),
                ));
            }
        };
        let (left_bp, right_bp) = get_op_binding_power(&op);
        if left_bp < min_bp {
            break;
        }
        iter.next();
        let rhs = parse_arithmetic_expr(iter, right_bp)?;
        lhs = Expression::BinaryExpr(op, vec![lhs, rhs])
    }
    Ok(lhs)
}
fn expect_token(
    iter: &mut Peekable<IntoIter<Token>>,
    expected: Token,
) -> Result<Token, ParseError> {
    match iter.next() {
        Some(tok) => {
            if tok != expected {
                return Err(ParseError::InvalidToken {
                    expected: Token::Return,
                    found: tok.clone(),
                });
            }
            Ok(tok)
        }
        None => return Err(ParseError::UnexpectedEnd),
    }
}
fn expect_id_token(iter: &mut Peekable<IntoIter<Token>>) -> Result<String, ParseError> {
    match iter.next() {
        Some(tok) => match tok.clone() {
            Token::Identifier(id) => Ok(id),
            t => {
                return Err(ParseError::InvalidToken {
                    expected: Token::Identifier("".to_string()),
                    found: t,
                });
            }
        },
        None => return Err(ParseError::UnexpectedEnd),
    }
}
pub fn parse_return_stmt(iter: &mut Peekable<IntoIter<Token>>) -> Result<ReturnStmt, ParseError> {
    expect_token(iter, Token::Return)?;

    let expr = parse_arithmetic_expr(iter, 0)?;

    expect_token(iter, Token::Semicolon)?;
    Ok(ReturnStmt { expression: expr })
}

pub fn parse_let_stmt(iter: &mut Peekable<IntoIter<Token>>) -> Result<LetStmt, ParseError> {
    expect_token(iter, Token::Let)?;

    let identifier = expect_id_token(iter)?;

    expect_token(iter, Token::Op(Operator::Assignment))?;

    let expr = parse_arithmetic_expr(iter, 0)?;

    expect_token(iter, Token::Semicolon)?;
    Ok(LetStmt {
        lhs: identifier,
        rhs: expr,
    })
}

pub fn parse_fn_decl(iter: &mut Peekable<IntoIter<Token>>) -> Result<FuncDecl, ParseError> {
    expect_token(iter, Token::Func)?;

    let func_name = expect_id_token(iter)?;

    expect_token(iter, Token::OpenParenthesis)?;
    //TODO: Impl parameter list parsing
    expect_token(iter, Token::CloseParenthesis)?;

    expect_token(iter, Token::Colon)?;

    //TODO: Impl return type parsing
    expect_token(iter, Token::Integer)?;
    expect_token(iter, Token::OpenBrace)?;

    let mut stmts = Vec::new();
    while let Some(tok) = iter.peek()
        && *tok != Token::CloseBrace
    {
        let t = tok.clone();
        if t == Token::Let {
            let stmt = parse_let_stmt(iter)?;
            stmts.push(Stmt::VarDecl(stmt));
        } else if t == Token::Return {
            let stmt = parse_return_stmt(iter)?;
            stmts.push(Stmt::Return(stmt));
        }
    }
    expect_token(iter, Token::CloseBrace)?;
    Ok(FuncDecl {
        name: func_name,
        field_list: None,
        body: BlockStmt { inner: stmts },
        return_type: String::from("i32"),
    })
}
