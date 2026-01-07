use crate::tokenizer::Operator;

pub enum AstNode {
    File(Program),
    FunctionDeclaration(FuncDecl),
    FunctionParameter(Field),
    BlockStatement(BlockStmt),
    VariableDeclaration(LetStmt),
    VariableAssignment(AssignStmt),
    Expression(Expression),
}
#[derive(Debug, Default)]
pub enum VarType {
    #[default]
    Unknown,
    Float32,
    Integer32,
    String,
}

// the whole program / file
pub struct Program {
    pub declarations: Vec<Declaration>,
}
// #[derive(PartialEq, Eq)]
pub enum Declaration {
    Func(FuncDecl),
    Var(LetStmt),
}
//func foo(param1: i32, param2: i16): i32 { return 67;}
#[derive(Debug)]
pub struct FuncDecl {
    pub name: String,
    pub field_list: Vec<Field>,
    pub body: BlockStmt,
    pub return_type: VarType,
}

//param1: i16
#[derive(Debug)]
pub struct Field {
    pub name: String,
    pub field_type: VarType,
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
    VarAssign(AssignStmt),
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

//let foo: i32 = 69;
#[derive(Debug)]
pub struct LetStmt {
    //the variable
    pub lhs: String,
    pub declared_type: VarType,
    //the value assigned to the variable
    pub rhs: Expression,
}

// foo = 69;
#[derive(Debug)]
pub struct AssignStmt {
    pub lhs: String,
    pub rhs: Expression,
}
