use std::{io::Error, iter::Peekable, vec::IntoIter};
use thiserror::Error;

use crate::{
    parser::ast::{Expression, Op, ReturnStmt},
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
fn get_op_binding_power(op: &Op) -> (u8, u8) {
    match op {
        Op::Add | Op::Sub => (1, 2),
        Op::Div | Op::Mul => (3, 4),
    }
}
fn token_op_to_node_op(tok: Token) -> Option<Op> {
    match tok {
        Token::Op(op) => match op {
            Operator::Addition => Some(Op::Add),
            Operator::Subtraction => Some(Op::Sub),
            Operator::Multiplication => Some(Op::Mul),
            Operator::Division => Some(Op::Div),
        },
        _ => None,
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
                expected: Token::NumberLiteral("any number".to_string()),
                found: t.unwrap_or(Token::Eof),
            });
        }
    };
    loop {
        let op = match iter.peek() {
            Some(Token::Eof | Token::Semicolon) => break,
            Some(Token::Op(op)) => token_op_to_node_op(Token::Op(op.clone())).unwrap(),
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
pub fn parse_return_stmt(iter: &mut Peekable<IntoIter<Token>>) -> Result<ReturnStmt, ParseError> {
    match iter.peek() {
        Some(tok) => {
            if *tok != Token::Return {
                return Err(ParseError::InvalidToken {
                    expected: Token::Return,
                    found: tok.clone(),
                });
            }
        }
        None => return Err(ParseError::UnexpectedEnd),
    };
    iter.next();
    let expr = parse_arithmetic_expr(iter, 0)?;

    Ok(ReturnStmt { expression: expr })
}
