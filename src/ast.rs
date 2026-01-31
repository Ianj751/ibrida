use std::fmt::{self, Display, Formatter};

use crate::tokenizer::Operator;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum VarType {
    #[default]
    Unknown,
    Float,
    Integer,

    Bool,
}
impl Display for VarType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = match self {
            VarType::Unknown => String::from("Unknown"),
            VarType::Float => String::from("f32"),
            VarType::Integer => String::from("i32"),

            VarType::Bool => String::from("bool"),
        };
        write!(f, "{}", name)
    }
}

// the whole program / file
#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}
#[derive(Debug, PartialEq, Eq)]
pub enum Declaration {
    Func(FuncDecl),
    Var(LetStmt),
}

//func foo(param1: i32, param2: i16): i32 { return 67;}
#[derive(Debug, PartialEq, Eq)]
pub struct FuncDecl {
    pub name: String,
    pub field_list: Vec<Field>,
    pub body: BlockStmt,
    pub decl_return_type: VarType,
}

//param1: i16
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub name: String,
    pub field_type: VarType,
}

// {let bar = 1.3; return 5;}
#[derive(Debug, PartialEq, Eq)]
pub struct BlockStmt {
    pub inner: Vec<Stmt>,
}
#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    Return(ReturnStmt),
    VarDecl(LetStmt),
    VarAssign(AssignStmt),
    IfStmt(IfStmt),
    Else(ElseStmt),
    FnCall(FuncCall),
}
#[derive(Debug, PartialEq, Eq)]
pub struct ReturnStmt {
    pub expression: Expression,
    pub checked_expr_type: Option<VarType>,
}

// 1 - 2 + 3 * 5;
#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    //the vartype here is for the checked type of the literal / id
    Literal(String, VarType), //leaf
    UnaryExpr {
        op: Operator,
        operand: Box<Expression>,
    }, //leaf
    Var(String, VarType),
    //FuncCall(String, VarType),
    BinaryExpr {
        op: Operator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    }, //parent, [lhs, rhs]
    FuncCall(FuncCall),
}

//let foo: i32 = 69;
#[derive(Debug, PartialEq, Eq)]
pub struct LetStmt {
    //the variable
    pub lhs: String,
    pub declared_type: VarType,
    //the value assigned to the variable
    pub rhs: Expression,
}

// foo = 69;
#[derive(Debug, PartialEq, Eq)]
pub struct AssignStmt {
    pub lhs: String,
    pub rhs: Expression,
    pub checked_expr_type: Option<VarType>,
}

// foo(a + b, c);
pub struct FunctionCall {
    func_id: String,
    fields: Vec<Expression>,
}
// if (foo == 2){
//  bar = 3;
//}
#[derive(Debug, PartialEq, Eq)]
pub struct IfStmt {
    pub body: BlockStmt,
    pub condition: Expression,
    pub else_stmt: Option<ElseStmt>,
}
// else{ bar = 2; }
// Maybe no need for elif. A nested else and if does the trick
#[derive(Debug, PartialEq, Eq)]
pub struct ElseStmt {
    pub body: BlockStmt,
}

// foo(a, 1, a+b);
#[derive(Debug, PartialEq, Eq)]
pub struct FuncCall {
    pub id: String,
    pub args: Vec<Expression>,
    pub return_type: Option<VarType>,
}
