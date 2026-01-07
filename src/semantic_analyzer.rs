use std::collections::HashMap;

use thiserror::Error;

use crate::ast::{
    AssignStmt, AstNode, BlockStmt, Declaration, Field, FuncDecl, LetStmt, Stmt, VarType,
};

/*
 * Things to check:
 * - The declared type of a variable is appropriate for its assignment and declaration
 *  * if contains decimal point, is float
 * - No multiple variable declarations
 * - No reference w/o declaration
 * - scope validation
 *   *have an array of symbol_tbls,index 0 is global. when come across a new scope append new one to array.
 *      When it ends, pop from end / clear ending symbol table.
 *      When looking up a symbol,
 *      first check from highest index symbol table to lowest
 */
/*
 * Produces an annotated AST w/ Validated Types
 *
 */
#[derive(Debug, Error)]
pub enum SemanticError {
    #[error("found symbol: {0} which was previously declared")]
    SymbolRedeclaration(String),
}
// uhh idk what
#[derive(Debug, Clone)]
struct Symbol {
    symbol_type: String,
    is_const: bool,
}

// The ast produced by the parser has the declared type
// the semantic analyzer will produce an ast with types that have been verified
type SymbolId = i32;
pub struct TypedNode {
    pub node: AstNode,
    pub node_type: VarType,
    pub symbol_id: Option<SymbolId>,
}

struct SemContext {
    scopes: Vec<HashMap<String, Symbol>>,
}
impl SemContext {
    pub fn new() -> Self {
        Self { scopes: Vec::new() }
    }
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    pub fn add_symbol(&mut self, name: &str, symbol_type: String, is_const: bool) {
        // this would not be a recoverable error, instead a programming fault
        let last = self.scopes.last_mut().unwrap();

        last.insert(
            name.to_string(),
            Symbol {
                symbol_type,
                is_const,
            },
        );
    }
    pub fn lookup_symbol(&self, name: &str) -> Option<Symbol> {
        for tbl in self.scopes.iter().rev() {
            if let Some(sym) = tbl.get(name) {
                return Some(sym.clone());
            }
        }
        None
    }
    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }
}

pub struct Visitor {
    sem_context: SemContext,
}
pub trait Visit<T> {
    fn visit(&mut self, visitable: &T);
}
impl Visit<Declaration> for Visitor {
    fn visit(&mut self, visitable: &Declaration) {
        match visitable {
            Declaration::Func(fn_decl) => self.visit(fn_decl),
            Declaration::Var(let_stmt) => self.visit(let_stmt),
        }
    }
}
impl Visit<FuncDecl> for Visitor {
    fn visit(&mut self, visitable: &FuncDecl) {
        let func_type = format!("func: {}", visitable.return_type.to_string());
        self.sem_context
            .add_symbol(&visitable.name, func_type, true);
        self.visit(&visitable.field_list);
        self.visit(&visitable.body);
    }
}
impl Visit<Vec<Field>> for Visitor {
    fn visit(&mut self, visitable: &Vec<Field>) {
        for param in visitable {
            self.sem_context
                .add_symbol(&param.name, param.field_type.to_string(), false);
        }
    }
}
impl Visit<BlockStmt> for Visitor {
    fn visit(&mut self, visitable: &BlockStmt) {
        self.sem_context.enter_scope();
        for stmt in &visitable.inner {
            match stmt {
                Stmt::Return(ret_stmt) => todo!("Compare type of return and function type"), //this should be verified against the return type of func
                Stmt::VarAssign(assign_stmt) => self.visit(assign_stmt),
                Stmt::VarDecl(let_stmt) => self.visit(let_stmt),
            }
        }
        self.sem_context.exit_scope();
    }
}
impl Visit<AssignStmt> for Visitor {
    fn visit(&mut self, visitable: &AssignStmt) {
        let sym = self.sem_context.lookup_symbol(&visitable.lhs);
        let ty = match sym {
            Some(s) => s.symbol_type,
            None => todo!(), // fail w/ err: assign w/o prev decl
        };
        //cmp type of sym and type of expr
        // if ne fail w/ err, fail w/ err: invalid assignmt
        todo!();
    }
}
impl Visit<LetStmt> for Visitor {
    fn visit(&mut self, visitable: &LetStmt) {
        todo!("compare type of expr and declared_type");
        self.sem_context
            .add_symbol(&visitable.lhs, visitable.declared_type.to_string(), true);
    }
}
