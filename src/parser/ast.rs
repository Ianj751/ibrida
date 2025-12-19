use crate::tokenizer::tokenizer::Operator;

type VarKind = String;

// the whole program / file
pub struct Program {}

//func foo(param1: i32, param2: i16): i32 { return 67;}
#[derive(Debug)]
pub struct FuncDecl {
    pub name: String,
    pub field_list: Option<Vec<Field>>,
    pub body: BlockStmt,
    pub return_type: VarKind,
}

//param1: i16
#[derive(Debug)]
pub struct Field {
    pub name: String,
    pub field_type: VarKind,
}

// {let bar = 1.3; return 5;}
#[derive(Debug)]
pub struct BlockStmt {
    pub inner: Vec<Stmt>,
}
#[derive(Debug)]
pub enum Stmt {
    Return(ReturnStmt),
    VarDecl(LetStmt),
}
#[derive(Debug)]
pub struct ReturnStmt {
    pub expression: Expression,
}

// 1 - 2 + 3 * 5;
#[derive(Debug)]
pub enum Expression {
    UnaryExpr(String),
    BinaryExpr(Operator, Vec<Expression>),
}

//let foo = 69;
#[derive(Debug)]
pub struct LetStmt {
    //the variable
    pub lhs: String,
    //the value assigned to the variable
    pub rhs: Expression,
}
