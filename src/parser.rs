use std::panic;
use std::{iter::Peekable, vec::IntoIter};
use thiserror::Error;

use crate::ast::{
    AssignStmt, BlockStmt, Declaration, ElseStmt, Expression, Field, FuncCall, FuncDecl, IfStmt,
    LetStmt, Program, ReturnStmt, Stmt, VarType,
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
    //denotes that couldnt find a token that fit the category  "expected"
    #[error("invalid token: expected any {expected}, but found {found:?}")]
    InvalidTokenKind { expected: String, found: Token },
    #[error("{0}")]
    CustomError(String),
}
//https://craftinginterpreters.com/parsing-expressions.html
// ones with 2 operands
// https://en.cppreference.com/w/c/language/operator_precedence.html
fn infix_binding_power(op: &Operator) -> (u8, u8) {
    match op {
        Operator::Addition | Operator::Subtraction => (22, 23),
        Operator::Division | Operator::Multiplication | Operator::Modulo => (24, 25),
        Operator::Less | Operator::LessEq | Operator::Greater | Operator::GreaterEq => (18, 19),
        Operator::BoolEq | Operator::NotEq => (16, 17),
        Operator::BoolAnd => (8, 9),
        Operator::BoolOr => (6, 7),
        Operator::Assignment => (2, 3),
        _ => panic!("bad op: {:?}", op),
    }
}
// ones with 1 operand
fn prefix_binding_power(op: &Operator) -> ((), u8) {
    match op {
        Operator::BoolNot => ((), 27),
        _ => panic!("bad op: {:?}", op),
    }
}
//this is kinda turning into an rvalue parser
// robbed from: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
fn parse_expression(
    iter: &mut Peekable<IntoIter<Token>>,
    min_bp: u8,
) -> Result<Expression, ParseError> {
    let mut lhs = match iter.next() {
        Some(Token::IntegerLiteral(x)) => Expression::Literal(x, VarType::Integer),
        Some(Token::FloatLiteral(x)) => Expression::Literal(x, VarType::Float),
        Some(Token::BoolLiteral(b)) => Expression::Literal(b.to_string(), VarType::Bool),
        Some(Token::Identifier(id)) => {
            if let Some(t) = iter.peek()
                && *t == Token::OpenParenthesis
            {
                let fn_call = parse_fn_call(iter, id)?;
                Expression::FuncCall(fn_call)
            } else {
                Expression::Var(id, VarType::Unknown)
            }
        }
        Some(Token::OpenParenthesis) => {
            let lhs = parse_expression(iter, 0)?;
            expect_token(iter, Token::CloseParenthesis)?;
            lhs
        }
        Some(Token::Op(op)) => {
            let ((), right_bp) = prefix_binding_power(&op);
            let rhs = parse_expression(iter, right_bp)?;
            Expression::UnaryExpr {
                op,
                operand: Box::new(rhs),
            }
        }
        Some(t) => {
            return Err(ParseError::InvalidTokenKind {
                expected: "expression".to_string(),
                found: t,
            });
        }
        None => {
            return Err(ParseError::CustomError(format!("uhhh here?")));
        }
    };

    loop {
        let op = match iter.peek() {
            Some(Token::Semicolon | Token::CloseParenthesis | Token::Comma) => {
                break;
            }
            Some(Token::Op(op)) => op.clone(),
            //?
            Some(t) => {
                return Err(ParseError::InvalidTokenKind {
                    expected: "expression".to_string(),
                    found: t.clone(),
                });
            }
            None => {
                return Err(ParseError::CustomError(String::from("here: 2")));
            }
        };
        let (left_bp, right_bp) = infix_binding_power(&op);
        if left_bp < min_bp {
            break;
        }
        iter.next();
        let rhs = parse_expression(iter, right_bp)?;
        lhs = Expression::BinaryExpr {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
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
            Token::Float32 => VarType::Float,
            Token::Bool => VarType::Bool,
            Token::Integer32 => VarType::Integer,

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
fn parse_block_stmt(iter: &mut Peekable<IntoIter<Token>>) -> Result<BlockStmt, ParseError> {
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
                let identifier = expect_id_token(iter)?;
                if let Some(x) = iter.peek()
                    && *x == Token::OpenParenthesis
                {
                    let stmt = parse_fn_call(iter, identifier)?;
                    expect_token(iter, Token::Semicolon)?;
                    stmts.push(Stmt::FnCall(stmt));
                } else {
                    let stmt = parse_assign_stmt(iter, identifier)?;
                    stmts.push(Stmt::VarAssign(stmt));
                }
            }
            Token::If => {
                let stmt = parse_if_stmt(iter)?;
                stmts.push(Stmt::IfStmt(stmt));
            }

            t => return Err(ParseError::CustomError(format!("hiii {:?}", t))),
        }
    }
    expect_token(iter, Token::CloseBrace)?;
    Ok(BlockStmt { inner: stmts })
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

    let body = parse_block_stmt(iter)?;

    Ok(FuncDecl {
        name: func_name,
        field_list: fields,
        body,
        decl_return_type: ret_type_decl,
    })
}

fn parse_assign_stmt(
    iter: &mut Peekable<IntoIter<Token>>,
    var_id: String,
) -> Result<AssignStmt, ParseError> {
    expect_token(iter, Token::Op(Operator::Assignment))?;

    let expr = parse_expression(iter, 0)?;
    expect_token(iter, Token::Semicolon)?;

    Ok(AssignStmt {
        lhs: var_id,
        rhs: expr,
        checked_expr_type: None,
    })
}
fn parse_if_stmt(iter: &mut Peekable<IntoIter<Token>>) -> Result<IfStmt, ParseError> {
    expect_token(iter, Token::If)?;

    //no need for parenthesis in the if cond
    // you can if you want to tho
    expect_token(iter, Token::OpenParenthesis)?;
    let condition = parse_expression(iter, 0)?;
    expect_token(iter, Token::CloseParenthesis)?;

    let body = parse_block_stmt(iter)?;

    if let Some(tok) = iter.peek()
        && *tok == Token::Else
    {
        let else_stmt = parse_else_stmt(iter)?;
        Ok(IfStmt {
            body,
            condition,
            else_stmt: Some(else_stmt),
        })
    } else {
        Ok(IfStmt {
            body,
            condition,
            else_stmt: None,
        })
    }
}
fn parse_else_stmt(iter: &mut Peekable<IntoIter<Token>>) -> Result<ElseStmt, ParseError> {
    expect_token(iter, Token::Else)?;
    let body = parse_block_stmt(iter)?;
    Ok(ElseStmt { body })
}
fn parse_fn_call(
    iter: &mut Peekable<IntoIter<Token>>,
    func_id: String,
) -> Result<FuncCall, ParseError> {
    expect_token(iter, Token::OpenParenthesis)?;
    let mut args = Vec::new();

    // if no args then this loop should skip??
    while let Some(tok) = iter.peek()
        && *tok != Token::CloseParenthesis
    {
        let expr = parse_expression(iter, 0)?;

        args.push(expr);
        match expect_token(iter, Token::Comma) {
            Ok(_) => continue,
            Err(ParseError::InvalidToken {
                expected: _,
                found: Token::CloseParenthesis,
            }) => break,
            Err(e) => return Err(e),
        }
    }

    //plan to reuse this in expr parsing so no semicolon eating
    //expect_token(iter, Token::Semicolon)?;
    Ok(FuncCall {
        id: func_id,
        args,
        return_type: None,
    })
}

pub fn parse_program(iter: &mut Peekable<IntoIter<Token>>) -> Result<Program, ParseError> {
    let mut declarations = Vec::new();
    loop {
        let decl = match iter.peek() {
            Some(tok) => match tok {
                Token::Func => {
                    let func_decl = parse_fn_decl(iter)?;
                    Declaration::Func(func_decl)
                }
                Token::Let => {
                    let var_decl = parse_let_stmt(iter)?;
                    Declaration::Var(var_decl)
                }
                Token::Eof => break,
                _ => return Err(ParseError::InvalidBeginningToken),
            },
            None => return Err(ParseError::UnexpectedEnd),
        };
        declarations.push(decl);
    }
    Ok(Program { declarations })
}

//I know these arent the best tests. Need to test sad path too
#[cfg(test)]
mod tests {
    use crate::tokenizer::BoolLit;

    use super::*;

    #[test]
    fn test_parse_expression() {
        // equivalent of:
        // 1 + 2 * 3.1;
        let tokens = vec![
            Token::IntegerLiteral("1".to_string()),
            Token::Op(Operator::Addition),
            Token::IntegerLiteral("2".to_string()),
            Token::Op(Operator::Multiplication),
            Token::FloatLiteral("3.1".to_string()),
            Token::Semicolon,
        ];
        let mut iter = tokens.into_iter().peekable();

        let expr = parse_expression(&mut iter, 0);
        let expected = Expression::BinaryExpr {
            op: Operator::Addition,

            lhs: Box::new(Expression::Literal(String::from("1"), VarType::Integer)),
            rhs: Box::new(Expression::BinaryExpr {
                op: Operator::Multiplication,

                lhs: Box::new(Expression::Literal(String::from("2"), VarType::Integer)),
                rhs: Box::new(Expression::Literal(String::from("3.1"), VarType::Float)),
            }),
        };

        assert_eq!(expr.unwrap(), expected);
    }
    #[test]
    fn test_parse_expression_paren() {
        //Equivalent of:
        //(1 + 2) * 3.1
        let tokens = vec![
            Token::OpenParenthesis,
            Token::IntegerLiteral("1".to_string()),
            Token::Op(Operator::Addition),
            Token::IntegerLiteral("2".to_string()),
            Token::CloseParenthesis,
            Token::Op(Operator::Multiplication),
            Token::FloatLiteral("3.1".to_string()),
            Token::Semicolon,
        ];
        let mut iter = tokens.into_iter().peekable();
        let received = parse_expression(&mut iter, 0);

        let expected = Expression::BinaryExpr {
            op: Operator::Multiplication,
            lhs: Box::new(Expression::BinaryExpr {
                op: Operator::Addition,
                lhs: Box::new(Expression::Literal("1".into(), VarType::Integer)),
                rhs: Box::new(Expression::Literal("2".into(), VarType::Integer)),
            }),
            rhs: Box::new(Expression::Literal("3.1".into(), VarType::Float)),
        };

        assert_eq!(expected, received.unwrap());
    }

    #[test]
    fn test_parse_return_stmt() {
        // equivalent of:
        // return 1.2 + foo;
        let tokens = vec![
            Token::Return,
            Token::FloatLiteral(String::from("1.2")),
            Token::Op(Operator::Addition),
            Token::Identifier(String::from("foo")),
            Token::Semicolon,
        ];
        let mut iter = tokens.into_iter().peekable();

        let ret_stmt = parse_return_stmt(&mut iter);
        let expected = ReturnStmt {
            expression: Expression::BinaryExpr {
                op: Operator::Addition,
                lhs: Box::new(Expression::Literal(String::from("1.2"), VarType::Float)),
                rhs: Box::new(Expression::Var(String::from("foo"), VarType::Unknown)),
            },
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
            Token::IntegerLiteral(String::from("1")),
            Token::Op(Operator::Division),
            Token::Identifier(String::from("bar")),
            Token::Semicolon,
        ];

        let mut iter = tokens.into_iter().peekable();

        let let_stmt = parse_let_stmt(&mut iter);
        let expected = LetStmt {
            lhs: String::from("foo"),
            declared_type: VarType::Float,
            rhs: Expression::BinaryExpr {
                op: Operator::Division,

                lhs: Box::new(Expression::Literal(String::from("1"), VarType::Integer)),
                rhs: Box::new(Expression::Var(String::from("bar"), VarType::Unknown)),
            },
        };

        assert_eq!(let_stmt.unwrap(), expected);
    }

    #[test]
    fn test_parse_fn_decl() {
        //equivalent of:
        // fn add(a: i32, b: i32): i32 { return a + b;}
        let tokens = vec![
            Token::Func,
            Token::Identifier(String::from("add")),
            Token::OpenParenthesis,
            Token::Identifier(String::from("a")),
            Token::Colon,
            Token::Integer32,
            Token::Comma,
            Token::Identifier(String::from("b")),
            Token::Colon,
            Token::Integer32,
            Token::CloseParenthesis,
            Token::Colon,
            Token::Integer32,
            Token::OpenBrace,
            Token::Return,
            Token::Identifier(String::from("a")),
            Token::Op(Operator::Addition),
            Token::Identifier(String::from("b")),
            Token::Semicolon,
            Token::CloseBrace,
        ];
        let mut iter = tokens.into_iter().peekable();
        let got = parse_fn_decl(&mut iter);
        let body = BlockStmt {
            inner: vec![Stmt::Return(ReturnStmt {
                expression: Expression::BinaryExpr {
                    op: Operator::Addition,
                    lhs: Box::new(Expression::Var(String::from("a"), VarType::Unknown)),
                    rhs: Box::new(Expression::Var(String::from("b"), VarType::Unknown)),
                },
                checked_expr_type: None,
            })],
        };
        let expected = FuncDecl {
            name: String::from("add"),
            field_list: vec![
                Field {
                    name: String::from("a"),
                    field_type: VarType::Integer,
                },
                Field {
                    name: String::from("b"),
                    field_type: VarType::Integer,
                },
            ],
            body,
            decl_return_type: VarType::Integer,
        };
        assert!(got.is_ok());
        assert_eq!(got.unwrap(), expected);
    }
    #[test]
    fn test_parse_assign_stmt() {
        // equivalent of:
        // foo = 69.1;
        let tokens = vec![
            Token::Op(Operator::Assignment),
            Token::FloatLiteral(String::from("69.1")),
            Token::Semicolon,
        ];
        let mut iter = tokens.into_iter().peekable();

        let assign_stmt = parse_assign_stmt(&mut iter, "foo".into());
        let expected = AssignStmt {
            lhs: String::from("foo"),
            rhs: Expression::Literal(String::from("69.1"), VarType::Float),
            checked_expr_type: None,
        };
        assert_eq!(assign_stmt.unwrap(), expected);
    }

    #[test]
    fn test_parse_program() {
        //equivalente de:
        // let foo: i32 = 67; func main(): i32 { return 0; }
        let tokens = vec![
            Token::Let,
            Token::Identifier(String::from("foo")),
            Token::Colon,
            Token::Integer32,
            Token::Op(Operator::Assignment),
            Token::IntegerLiteral(String::from("67")),
            Token::Semicolon,
            Token::Func,
            Token::Identifier(String::from("main")),
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::Colon,
            Token::Integer32,
            Token::OpenBrace,
            Token::Return,
            Token::IntegerLiteral(String::from("0")),
            Token::Semicolon,
            Token::CloseBrace,
            Token::Eof,
        ];
        let mut iter = tokens.into_iter().peekable();
        let prog = parse_program(&mut iter);
        let expected = Program {
            declarations: vec![
                Declaration::Var(LetStmt {
                    lhs: String::from("foo"),
                    declared_type: VarType::Integer,
                    rhs: Expression::Literal(String::from("67"), VarType::Integer),
                }),
                Declaration::Func(FuncDecl {
                    name: String::from("main"),
                    field_list: Vec::new(),
                    body: BlockStmt {
                        inner: vec![Stmt::Return(ReturnStmt {
                            expression: Expression::Literal(String::from("0"), VarType::Integer),
                            checked_expr_type: None,
                        })],
                    },
                    decl_return_type: VarType::Integer,
                }),
            ],
        };
        assert_eq!(expected, prog.unwrap());
    }
    #[test]
    fn test_parse_prefix_expr() {
        //Equivalent of:
        // foo + !true;
        let tokens = vec![
            Token::Identifier(String::from("foo")),
            Token::Op(Operator::Addition),
            Token::Op(Operator::BoolNot),
            Token::BoolLiteral(BoolLit::True),
            Token::Semicolon,
        ];
        let mut iter = tokens.into_iter().peekable();
        let recv = parse_expression(&mut iter, 0).unwrap();

        let expected = Expression::BinaryExpr {
            op: Operator::Addition,
            lhs: Box::new(Expression::Var(String::from("foo"), VarType::Unknown)),
            rhs: Box::new(Expression::UnaryExpr {
                op: Operator::BoolNot,
                operand: Box::new(Expression::Literal("true".to_string(), VarType::Bool)),
            }),
        };

        assert_eq!(expected, recv);
    }
    #[test]
    fn test_parse_if_stmt() {
        //Equivalent of:
        //if (z < 4){foo = 1 + 2;}
        let tokens = vec![
            Token::If,
            Token::OpenParenthesis,
            Token::Identifier(String::from("z")),
            Token::Op(Operator::Less),
            Token::IntegerLiteral(String::from("4")),
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::Identifier(String::from("foo")),
            Token::Op(Operator::Assignment),
            Token::IntegerLiteral(String::from("1")),
            Token::Op(Operator::Addition),
            Token::IntegerLiteral(String::from("2")),
            Token::Semicolon,
            Token::CloseBrace,
        ];
        let body = BlockStmt {
            inner: vec![Stmt::VarAssign(AssignStmt {
                lhs: "foo".to_string(),
                rhs: Expression::BinaryExpr {
                    op: Operator::Addition,
                    lhs: Box::new(Expression::Literal("1".to_string(), VarType::Integer)),
                    rhs: Box::new(Expression::Literal("2".to_string(), VarType::Integer)),
                },
                checked_expr_type: None,
            })],
        };
        let condition = Expression::BinaryExpr {
            op: Operator::Less,
            lhs: Box::new(Expression::Var("z".into(), VarType::Unknown)),
            rhs: Box::new(Expression::Literal("4".to_string(), VarType::Integer)),
        };
        let expected = IfStmt {
            body,
            condition,
            else_stmt: None,
        };
        let mut iter = tokens.into_iter().peekable();
        let if_stmt = parse_if_stmt(&mut iter);

        assert_eq!(expected, if_stmt.unwrap());
    }
    #[test]
    fn test_parse_fn_call_noargs() {
        //Equivalent of:
        // foo();
        let tokens = vec![
            Token::OpenParenthesis,
            Token::CloseParenthesis,
            Token::Semicolon,
        ];

        let mut iter = tokens.into_iter().peekable();
        let received = parse_fn_call(&mut iter, "foo".into());

        let expected = FuncCall {
            id: String::from("foo"),
            args: Vec::new(),
            return_type: None,
        };
        assert_eq!(expected, received.unwrap());
    }
    #[test]
    fn test_parse_fn_call_args() {
        //Equivalent of:
        //foo(1, a + b, false)
        let tokens = vec![
            Token::OpenParenthesis,
            Token::IntegerLiteral("1".into()),
            Token::Comma,
            Token::Identifier("a".into()),
            Token::Op(Operator::Addition),
            Token::Identifier("b".into()),
            Token::Comma,
            Token::BoolLiteral(BoolLit::False),
            Token::CloseParenthesis,
        ];
        let mut iter = tokens.into_iter().peekable();
        let recieved = parse_fn_call(&mut iter, "foo".into());

        let expected = FuncCall {
            id: "foo".into(),
            args: vec![
                Expression::Literal("1".into(), VarType::Integer),
                Expression::BinaryExpr {
                    op: Operator::Addition,
                    lhs: Box::new(Expression::Var("a".into(), VarType::Unknown)),
                    rhs: Box::new(Expression::Var("b".into(), VarType::Unknown)),
                },
                Expression::Literal("false".into(), VarType::Bool),
            ],
            return_type: None,
        };

        assert_eq!(expected, recieved.unwrap());
    }
}
