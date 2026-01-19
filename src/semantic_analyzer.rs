use std::collections::HashMap;

use thiserror::Error;

use crate::{
    ast::{
        AssignStmt, BlockStmt, Declaration, Expression, Field, FuncDecl, LetStmt, Program,
        ReturnStmt, Stmt, VarType,
    },
    tokenizer::Operator,
};

#[derive(Debug, Error)]
pub enum SemanticError {
    #[error("found symbol: {0} which was previously declared")]
    SymbolRedeclaration(String),
    #[error("Unable to assign to symbol {0}")]
    InvalidAssignment(String),
    #[error("invalid symbol reference: found {0} which was referenced without being initialized")]
    InvalidSymbolReference(String),
    #[error("invalid operand types for operator {0:?}")]
    InvalidOperands(Operator),
    #[error("Unexpected Value: {0}")] //idk what to put here. Kind of a generic error ig
    UnexpectedValue(String),
    #[error("Mismatched Types: got {found} but expected {expected}")]
    MismatchedTypes { expected: String, found: String },
    #[error(
        "Invalid Return Statement: expected return statement of type {expected} but found {found} "
    )]
    InvalidFunctionReturn { expected: String, found: String },
    #[error("{0}")]
    CustomError(String),
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub symbol_type: String,
    pub is_const: bool,
}

struct SymbolTable {
    //list of indicies of the children of the given scope / symbol table
    children: Option<Vec<usize>>,
    parent: Option<usize>,
    symbols: HashMap<String, Symbol>,
}
pub struct SemContext {
    scopes: Vec<SymbolTable>,
    // index pointing to the current scope to which new symbols will be added
    curr_scope: usize,
}

impl SemContext {
    pub fn new() -> Self {
        Self {
            scopes: vec![SymbolTable {
                children: None,
                parent: None,
                symbols: HashMap::new(),
            }],
            curr_scope: 0,
        }
    }

    pub fn enter_scope(&mut self) {
        // add a new scope
        self.scopes.push(SymbolTable {
            children: None,
            parent: Some(self.curr_scope),
            symbols: HashMap::new(),
        });

        let child_idx = self.scopes.len() - 1;

        self.scopes[self.curr_scope]
            .children
            .get_or_insert_with(Vec::new)
            .push(child_idx);

        self.curr_scope = child_idx;
    }
    pub fn add_symbol(&mut self, name: &str, symbol_type: String, is_const: bool) {
        self.scopes[self.curr_scope].symbols.insert(
            name.to_string(),
            Symbol {
                symbol_type,
                is_const,
            },
        );
    }
    pub fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        if let Some(sym) = self.scopes[self.curr_scope].symbols.get(name) {
            return Some(sym);
        }
        let mut idx = self.scopes[self.curr_scope].parent?;

        loop {
            if let Some(sym) = self.scopes[idx].symbols.get(name) {
                return Some(sym);
            }
            if idx == 0 {
                break;
            }
            idx = self.scopes[idx].parent?;
        }
        None
    }

    pub fn exit_scope(&mut self) {
        //if i unwrap on a None then I'm either at the GlobalScope or I messed up,
        // either way this is not a recoverable error
        self.curr_scope = self.scopes[self.curr_scope]
            .parent
            .expect("attempted to exit a parentless scope");
    }
}
// SA is short for semantic analyzer
// follows visitorpattern and embeds type info on the AST inplace
pub struct SAVisitor {
    sem_context: SemContext,
    //maintains a LIFO Queue of the current function return type so that I can
    // compare the value of return stmt to the decl return type w/o violating visitor pattern
    func_ret_stack: Vec<VarType>,
}
impl SAVisitor {
    pub fn new() -> Self {
        Self {
            sem_context: SemContext::new(),
            func_ret_stack: Vec::new(),
        }
    }
}
type SAResult = Result<(), SemanticError>;
pub trait Visit<T> {
    fn visit(&mut self, visitable: &mut T) -> SAResult;
}
impl Visit<Program> for SAVisitor {
    fn visit(&mut self, visitable: &mut Program) -> SAResult {
        for decl in &mut visitable.declarations {
            self.visit(decl)?;
        }
        Ok(())
    }
}
impl Visit<Declaration> for SAVisitor {
    fn visit(&mut self, visitable: &mut Declaration) -> SAResult {
        match visitable {
            Declaration::Func(fn_decl) => self.visit(fn_decl),
            Declaration::Var(let_stmt) => self.visit(let_stmt),
        }
    }
}
impl Visit<FuncDecl> for SAVisitor {
    fn visit(&mut self, visitable: &mut FuncDecl) -> SAResult {
        self.func_ret_stack.push(visitable.decl_return_type.clone());

        let func_type = format!("func: {}", visitable.decl_return_type);
        self.sem_context
            .add_symbol(&visitable.name, func_type, true);

        self.visit(&mut visitable.field_list)?;
        self.visit(&mut visitable.body)?;

        self.func_ret_stack.pop();
        Ok(())
    }
}
impl Visit<Vec<Field>> for SAVisitor {
    fn visit(&mut self, visitable: &mut Vec<Field>) -> SAResult {
        for param in visitable {
            self.sem_context
                .add_symbol(&param.name, param.field_type.to_string(), false);
        }
        Ok(())
    }
}
impl Visit<BlockStmt> for SAVisitor {
    fn visit(&mut self, visitable: &mut BlockStmt) -> SAResult {
        self.sem_context.enter_scope();
        for stmt in &mut visitable.inner {
            match stmt {
                Stmt::Return(ret_stmt) => self.visit(ret_stmt), //this should be verified against the return type of func
                Stmt::VarAssign(assign_stmt) => self.visit(assign_stmt),
                Stmt::VarDecl(let_stmt) => self.visit(let_stmt),
            }?;
        }
        self.sem_context.exit_scope();

        Ok(())
    }
}
impl Visit<AssignStmt> for SAVisitor {
    fn visit(&mut self, visitable: &mut AssignStmt) -> SAResult {
        let sym = self.sem_context.lookup_symbol(&visitable.lhs);
        let ty = match sym {
            Some(s) => {
                if s.is_const {
                    return Err(SemanticError::InvalidAssignment(visitable.lhs.clone()));
                }
                &s.symbol_type
            }
            None => return Err(SemanticError::InvalidSymbolReference(visitable.lhs.clone())),
        };

        let expr_ty = typecheck_expr(&mut visitable.rhs, &self.sem_context)?;
        if expr_ty.to_string() != *ty {
            return Err(SemanticError::MismatchedTypes {
                found: expr_ty.to_string(),
                expected: ty.clone(),
            });
        }

        visitable.checked_expr_type = Some(expr_ty);
        Ok(())
    }
}
impl Visit<LetStmt> for SAVisitor {
    fn visit(&mut self, visitable: &mut LetStmt) -> SAResult {
        if self.sem_context.lookup_symbol(&visitable.lhs).is_some() {
            return Err(SemanticError::SymbolRedeclaration(visitable.lhs.clone()));
        }

        self.sem_context
            .add_symbol(&visitable.lhs, visitable.declared_type.to_string(), false);
        let expr_ty = typecheck_expr(&mut visitable.rhs, &self.sem_context)?;

        if visitable.declared_type != expr_ty {
            return Err(SemanticError::MismatchedTypes {
                expected: visitable.declared_type.to_string(),
                found: expr_ty.to_string(),
            });
        }

        Ok(())
    }
}
impl Visit<ReturnStmt> for SAVisitor {
    fn visit(&mut self, visitable: &mut ReturnStmt) -> SAResult {
        let last = self.func_ret_stack.last();
        if last.is_none() {
            return Err(SemanticError::InvalidFunctionReturn {
                expected: String::from("None"),
                found: String::from("Some"), //this is a bad error
            });
        }

        let last = last.unwrap();
        let expr_ty = typecheck_expr(&mut visitable.expression, &self.sem_context)?;
        if *last != expr_ty {
            return Err(SemanticError::InvalidFunctionReturn {
                expected: last.to_string(),
                found: expr_ty.to_string(),
            });
        }
        visitable.checked_expr_type = Some(expr_ty);
        Ok(())
    }
}

// returns the overall type of the expression and embeds types on the Expression tree
fn typecheck_expr(expr: &mut Expression, sem_ctx: &SemContext) -> Result<VarType, SemanticError> {
    match expr {
        Expression::BinaryExpr(op, children) => {
            //parser should have already verified that each operator has 2 operands
            assert!(children.len() == 2);
            let mut child_types = [VarType::Unknown, VarType::Unknown];

            for (i, child) in children.iter_mut().enumerate() {
                let ty = typecheck_expr(child, sem_ctx)?;
                assert!(ty != VarType::Unknown); //would be dumb if it was unknown
                child_types[i] = ty;
            }
            match op {
                Operator::Addition
                | Operator::Multiplication
                | Operator::Division
                | Operator::Subtraction => {
                    if child_types[0] == child_types[1] {
                        match &child_types[0] {
                            VarType::String => Err(SemanticError::InvalidOperands(op.clone())),
                            t => Ok(t.clone()),
                        }
                    } else if child_types.contains(&VarType::Integer)
                        && child_types.contains(&VarType::Float)
                    {
                        Ok(VarType::Float)
                    } else {
                        Err(SemanticError::InvalidOperands(op.clone()))
                    }
                }
                _ => Err(SemanticError::InvalidOperands(op.clone())),
            }
        }
        Expression::UnaryExpr(id, ty) => match ty {
            VarType::Unknown => match sem_ctx.lookup_symbol(id) {
                Some(s) => {
                    //functions are represented in the symbol table as "func: <return_type>"
                    if s.symbol_type.ends_with("f32") {
                        *ty = VarType::Float;
                        Ok(VarType::Float)
                    } else if s.symbol_type.ends_with("i32") {
                        *ty = VarType::Integer;
                        Ok(VarType::Integer)
                    } else if s.symbol_type.ends_with("string") {
                        *ty = VarType::String;
                        Ok(VarType::String)
                    } else {
                        Err(SemanticError::UnexpectedValue(s.symbol_type.clone())) //more of a programmer error ig
                    }
                }
                None => Err(SemanticError::InvalidSymbolReference(id.clone())),
            },
            t => Ok(t.clone()),
        },
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;

    #[test]
    fn test_typecheck_expr_basic_add() {
        let mut expr = Expression::BinaryExpr(
            Operator::Addition,
            vec![
                Expression::UnaryExpr(String::from("1"), VarType::Integer),
                Expression::UnaryExpr(String::from("2.1"), VarType::Float),
            ],
        );
        let sem_ctx = SemContext::new();
        let got = typecheck_expr(&mut expr, &sem_ctx);
        assert!(got.is_ok());

        assert_eq!(got.unwrap(), VarType::Float);
    }
    #[test]
    fn test_typecheck_expr_ast_embed() {
        let mut expr = Expression::BinaryExpr(
            Operator::Addition,
            vec![
                Expression::UnaryExpr(String::from("1"), VarType::Integer),
                Expression::UnaryExpr(String::from("foo"), VarType::Unknown),
            ],
        );
        let mut sem_ctx = SemContext::new();
        sem_ctx.add_symbol("foo", String::from("f32"), false);

        let got = typecheck_expr(&mut expr, &sem_ctx);
        assert!(got.is_ok());

        match expr {
            Expression::BinaryExpr(_, v) => match v.get(1) {
                Some(Expression::UnaryExpr(_, ty)) => assert!(*ty == VarType::Float),
                _ => panic!("How did we even get here? Expected UnaryExpr at index 1"),
            },
            _ => panic!("How did we even get here? Expected BinaryExpr but got Unary"),
        }

        assert_eq!(got.unwrap(), VarType::Float);
    }
    #[test]
    fn test_sem_ctx_lookup() {
        /* Equivalent of:
            fn add():i32 {...}
            func main(): i32{
                let a: i32..
                {
                    let b: f32..
                    add(a, b); //lookup symbols here

                }
            }
        */
        let mut sem_ctx = SemContext::new();
        sem_ctx.add_symbol("add", String::from("func: i32"), true);
        sem_ctx.add_symbol("main", String::from("func: i32"), true);
        sem_ctx.enter_scope();
        sem_ctx.add_symbol("a", String::from("i32"), false);
        sem_ctx.enter_scope();
        sem_ctx.add_symbol("b", String::from("i32"), false);

        assert!(sem_ctx.lookup_symbol("add").is_some());
        assert!(sem_ctx.lookup_symbol("a").is_some());
        assert!(sem_ctx.lookup_symbol("b").is_some());

        sem_ctx.exit_scope();
        assert!(sem_ctx.lookup_symbol("b").is_none());
        sem_ctx.exit_scope();
        assert!(sem_ctx.lookup_symbol("a").is_none());
    }
}
