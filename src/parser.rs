use std::{iter::Peekable, vec::IntoIter};
use thiserror::Error;

use crate::ast::{
    AssignStmt, BlockStmt, Expression, Field, FuncDecl, LetStmt, ReturnStmt, Stmt, VarType,
};
use crate::tokenizer::{Operator, Token};

#[derive(Debug, Error, PartialEq, Eq)]
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
//this is kinda turning into an rvalue parser
fn parse_expression(
    iter: &mut Peekable<IntoIter<Token>>,
    min_bp: u8,
) -> Result<Expression, ParseError> {
    let mut lhs = match iter.next() {
        Some(Token::NumberLiteral(x)) => Expression::UnaryExpr(x, None),
        Some(Token::Identifier(id)) => Expression::UnaryExpr(id, None),
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
        let rhs = parse_expression(iter, right_bp)?;
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
                    expected,
                    found: tok.clone(),
                });
            }
            Ok(tok)
        }
        None => Err(ParseError::UnexpectedEnd),
    }
}
fn expect_id_token(iter: &mut Peekable<IntoIter<Token>>) -> Result<String, ParseError> {
    match iter.next() {
        Some(tok) => match tok.clone() {
            Token::Identifier(id) => Ok(id),
            t => Err(ParseError::InvalidToken {
                expected: Token::Identifier("".to_string()),
                found: t,
            }),
        },
        None => Err(ParseError::UnexpectedEnd),
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

    let expr = parse_expression(iter, 0)?;

    expect_token(iter, Token::Semicolon)?;
    Ok(ReturnStmt {
        expression: expr,
        checked_expr_type: None,
    })
}

pub fn parse_let_stmt(iter: &mut Peekable<IntoIter<Token>>) -> Result<LetStmt, ParseError> {
    expect_token(iter, Token::Let)?;

    let identifier = expect_id_token(iter)?;
    expect_token(iter, Token::Colon)?;
    let var_type = expect_type_token(iter)?;

    expect_token(iter, Token::Op(Operator::Assignment))?;

    let expr = parse_expression(iter, 0)?;

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
            name,
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
        decl_return_type: ret_type_decl,
    })
}

fn parse_assign_stmt(iter: &mut Peekable<IntoIter<Token>>) -> Result<AssignStmt, ParseError> {
    let identifier = expect_id_token(iter)?;
    expect_token(iter, Token::Op(Operator::Assignment))?;

    let expr = parse_expression(iter, 0)?;
    expect_token(iter, Token::Semicolon)?;

    Ok(AssignStmt {
        lhs: identifier,
        rhs: expr,
        checked_expr_type: None,
    })
}

//I know these arent the best tests. Need to test sad path too
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_expression() {
        // equivalent of:
        // 1 + 2 * 3;
        let tokens = vec![
            Token::NumberLiteral("1".to_string()),
            Token::Op(Operator::Addition),
            Token::NumberLiteral("2".to_string()),
            Token::Op(Operator::Multiplication),
            Token::NumberLiteral("3".to_string()),
            Token::Semicolon,
        ];
        let mut iter = tokens.into_iter().peekable();

        let expr = parse_expression(&mut iter, 0);
        let expected = Expression::BinaryExpr(
            Operator::Addition,
            vec![
                Expression::UnaryExpr(String::from("1"), None),
                Expression::BinaryExpr(
                    Operator::Multiplication,
                    vec![
                        Expression::UnaryExpr(String::from("2"), None),
                        Expression::UnaryExpr(String::from("3"), None),
                    ],
                ),
            ],
        );

        assert_eq!(expr.unwrap(), expected);
    }

    #[test]
    fn test_parse_return_stmt() {
        // equivalent of:
        // return 1 + foo;
        let tokens = vec![
            Token::Return,
            Token::NumberLiteral(String::from("1")),
            Token::Op(Operator::Addition),
            Token::Identifier(String::from("foo")),
            Token::Semicolon,
        ];
        let mut iter = tokens.into_iter().peekable();

        let ret_stmt = parse_return_stmt(&mut iter);
        let expected = ReturnStmt {
            expression: Expression::BinaryExpr(
                Operator::Addition,
                vec![
                    Expression::UnaryExpr(String::from("1"), None),
                    Expression::UnaryExpr(String::from("foo"), None),
                ],
            ),
            checked_expr_type: None,
        };
        assert_eq!(ret_stmt.unwrap(), expected);
    }

    #[test]
    fn test_parse_let_stmt() {
        //equivalent of:
        // let foo: f32 = 1 / bar;
        let tokens = vec![
            Token::Let,
            Token::Identifier(String::from("foo")),
            Token::Colon,
            Token::Float32,
            Token::Op(Operator::Assignment),
            Token::NumberLiteral(String::from("1")),
            Token::Op(Operator::Division),
            Token::Identifier(String::from("bar")),
            Token::Semicolon,
        ];

        let mut iter = tokens.into_iter().peekable();

        let let_stmt = parse_let_stmt(&mut iter);
        let expected = LetStmt {
            lhs: String::from("foo"),
            declared_type: VarType::Float32,
            rhs: Expression::BinaryExpr(
                Operator::Division,
                vec![
                    Expression::UnaryExpr(String::from("1"), None),
                    Expression::UnaryExpr(String::from("bar"), None),
                ],
            ),
        };

        assert_eq!(let_stmt.unwrap(), expected);
    }

    #[test]
    fn test_parse_fn_decl() {
        //equivalent of:
        // fn main(): i32 { return 1 + 2;}
        let tokens = vec![
            Token::Func,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::Colon,
            Token::Integer32,
            Token::OpenBrace,
            Token::Return,
            Token::NumberLiteral(String::from("1")),
            Token::Op(Operator::Addition),
            Token::NumberLiteral(String::from("2")),
            Token::Semicolon,
            Token::CloseBrace,
        ];
        let mut iter = tokens.into_iter().peekable();
        let fn_decl = parse_fn_decl(&mut iter);
        let body = BlockStmt {
            inner: vec![Stmt::Return(ReturnStmt {
                expression: Expression::BinaryExpr(
                    Operator::Addition,
                    vec![
                        Expression::UnaryExpr(String::from("1"), None),
                        Expression::UnaryExpr(String::from("2"), None),
                    ],
                ),
                checked_expr_type: None,
            })],
        };
        let expected = FuncDecl {
            name: String::from("main"),
            field_list: Vec::new(),
            body,
            decl_return_type: VarType::Integer32,
        };

        assert_eq!(fn_decl.unwrap(), expected);
    }
    #[test]
    fn test_parse_assign_stmt() {
        // equivalent of:
        // foo = 69;
        let tokens = vec![
            Token::Identifier(String::from("foo")),
            Token::Op(Operator::Assignment),
            Token::NumberLiteral(String::from("69")),
            Token::Semicolon,
        ];
        let mut iter = tokens.into_iter().peekable();

        let assign_stmt = parse_assign_stmt(&mut iter);
        let expected = AssignStmt {
            lhs: String::from("foo"),
            rhs: Expression::UnaryExpr(String::from("69"), None),
            checked_expr_type: None,
        };
        assert_eq!(assign_stmt.unwrap(), expected);
    }
}
