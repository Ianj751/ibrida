trait Node {
    // dont know which specific type it is, just that its a node
    fn get_value(&self) -> Box<dyn Node>;
}

//ReturnStmt contains an expression which may be arithmetic or literally anything else
// confining to arithmetic for the moment
#[derive(Debug)]
pub struct ReturnStmt {
    pub expression: Expression,
}
#[derive(Debug)]
pub enum Expression {
    UnaryExpr(String),
    BinaryExpr(Op, Vec<Expression>),
}
#[derive(Debug)]
pub enum Op {
    Add,
    Sub,
    Div,
    Mul,
    // Mod,
    //  Assign,
    //Dot,
}
