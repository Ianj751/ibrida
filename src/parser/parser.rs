use std::{iter::Peekable, vec::IntoIter};
use thiserror::Error;

use crate::{
    parser::ast::{
        AssignStmt, BlockStmt, Expression, Field, FuncDecl, LetStmt, ReturnStmt, Stmt, VarType,
    },
    tokenizer::{Token, tokenizer::Operator},
};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("invalid token: expected {expected:?} but found {found:?}")]
    InvalidToken { expected: Token, found: Token },
    #[error("invalid token: expected an operator, but found `{0:?}`")]
    ExpectedOperator(Token),
    #[error("invalid token: expected a variable type, but found `{0:?}`")]
    ExpectedType(Token),
    #[error("unexpected end of token stream")]
    UnexpectedEnd,
    #[error("invalid token to begin the line")] //idk i need a better error for this one
    InvalidBeginningToken,
}
//https://craftinginterpreters.com/parsing-expressions.html

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
                    expected: expected,
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

fn expect_type_token(iter: &mut Peekable<IntoIter<Token>>) -> Result<VarType, ParseError> {
    let kind = match iter.next() {
        Some(tok) => match tok.clone() {
            Token::Float32 => VarType::Float32,
            Token::Integer32 => VarType::Integer32,
            Token::String => VarType::String,
            t => return Err(ParseError::ExpectedType(t)),
        },
        None => return Err(ParseError::UnexpectedEnd),
    };
    Ok(kind)
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
    expect_token(iter, Token::Colon)?;
    let var_type = expect_type_token(iter)?;

    expect_token(iter, Token::Op(Operator::Assignment))?;

    let expr = parse_arithmetic_expr(iter, 0)?;

    expect_token(iter, Token::Semicolon)?;
    Ok(LetStmt {
        lhs: identifier,
        declared_type: var_type,
        rhs: expr,
    })
}

pub fn parse_fn_decl(iter: &mut Peekable<IntoIter<Token>>) -> Result<FuncDecl, ParseError> {
    expect_token(iter, Token::Func)?;

    let func_name = expect_id_token(iter)?;

    expect_token(iter, Token::OpenParenthesis)?;
    let mut fields: Vec<Field> = Vec::new();
    while let Some(tok) = iter.peek()
        && *tok != Token::CloseParenthesis
    {
        let name = expect_id_token(iter)?;
        expect_token(iter, Token::Colon)?;
        let var_type = expect_type_token(iter)?;
        fields.push(Field {
            name: name,
            field_type: var_type,
        });
        //expect comma if this is the only field.
        // if no comma break, else consume comma
        if let Some(tok) = iter.peek()
            && *tok != Token::Comma
        {
            break;
        }
        expect_token(iter, Token::Comma)?;
    }
    expect_token(iter, Token::CloseParenthesis)?;

    expect_token(iter, Token::Colon)?;

    let ret_type_decl = expect_type_token(iter)?;
    expect_token(iter, Token::OpenBrace)?;

    let mut stmts = Vec::new();
    while let Some(tok) = iter.peek()
        && *tok != Token::CloseBrace
    {
        let t = tok.clone();

        match t {
            Token::Let => {
                let stmt = parse_let_stmt(iter)?;
                stmts.push(Stmt::VarDecl(stmt));
            }
            Token::Return => {
                let stmt = parse_return_stmt(iter)?;
                stmts.push(Stmt::Return(stmt));
            }
            Token::Identifier(_) => {
                // FIXME: when adding function call parsing, consume identifier then based on peek,
                // if find OpeningParenthesis, parse func_call else assign_stmt
                let stmt = parse_assign_stmt(iter)?;
                stmts.push(Stmt::VarAssign(stmt));
            }
            _ => return Err(ParseError::InvalidBeginningToken),
        }
    }
    expect_token(iter, Token::CloseBrace)?;
    Ok(FuncDecl {
        name: func_name,
        field_list: fields,
        body: BlockStmt { inner: stmts },
        return_type: ret_type_decl,
    })
}

fn parse_assign_stmt(iter: &mut Peekable<IntoIter<Token>>) -> Result<AssignStmt, ParseError> {
    let identifier = expect_id_token(iter)?;
    expect_token(iter, Token::Op(Operator::Assignment))?;

    let expr = parse_arithmetic_expr(iter, 0)?;
    expect_token(iter, Token::Semicolon)?;

    Ok(AssignStmt {
        lhs: identifier,
        rhs: expr,
    })
}
