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
    #[error("found symbol: {0} which was assigned and not previously")]
    InvalidAssignment(String),
}
// uhh idk what
#[derive(Debug, Clone)]
struct Symbol {
    symbol_type: String,
    is_const: bool,
}

// The ast produced by the parser has the declared type
// the semantic analyzer will produce an ast with types that have been verified

pub struct TypedNode {
    pub node: AstNode,
    pub node_type: String, //Not a fan of this, may change to an enum
                           // pub symbol_id: Option<SymbolId>,
}
struct SymbolTable {
    //list of indicies of the children of the given scope / symbol table
    children: Option<Vec<usize>>,
    parent: Option<usize>,
    symbols: HashMap<String, Symbol>,
}
struct SemContext {
    pub scopes: Vec<SymbolTable>,
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
        self.scopes.push(SymbolTable {
            children: None,
            parent: Some(self.curr_scope),
            symbols: HashMap::new(),
        });
        let child_idx = self.scopes.len() - 1;
        // match &mut self.scopes[self.curr_scope].children {
        //     Some(c) => c.push(child_idx),
        //     None => self.scopes[self.curr_scope].children = Some(vec![child_idx]),
        // }
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
        let idx = match self.scopes[self.curr_scope].parent {
            Some(i) => i,
            None => return self.scopes[self.curr_scope].symbols.get(name),
        };

        //breaks at index: 0 b/c GlobalScope parent is_none
        // so the second condition is a little redundant
        while let idx = self.scopes[idx].parent
            && (idx.is_some() || idx != Some(0))
        {
            let tbl = &self.scopes[idx.unwrap()];
            if let Some(sym) = tbl.symbols.get(name) {
                return Some(sym);
            }
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
// follows visitorpattern and embeds type info on the AST
pub struct SAVisitor {
    sem_context: SemContext,
    //validator: TypeValidator, Wraps hashmap containing
}
type SAResult = Result<(), SemanticError>;
pub trait Visit<T> {
    fn visit(&mut self, visitable: &mut T) -> SAResult;
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
        let func_type = format!("func: {}", visitable.decl_return_type);
        self.sem_context
            .add_symbol(&visitable.name, func_type, true);
        self.visit(&mut visitable.field_list)?;
        self.visit(&mut visitable.body)?;
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
                Stmt::Return(_) => todo!("Compare type of return and function type"), //this should be verified against the return type of func
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
            Some(s) => &s.symbol_type,
            None => return Err(SemanticError::InvalidAssignment(visitable.lhs.clone())),
        };
        //cmp type of sym and type of expr
        // if ne fail w/ err, fail w/ err: invalid assignmt
        todo!();
    }
}
impl Visit<LetStmt> for SAVisitor {
    fn visit(&mut self, visitable: &mut LetStmt) -> SAResult {
        todo!("compare type of expr and declared_type");
        //Will have default mutability until refine default mutability logic
        self.sem_context
            .add_symbol(&visitable.lhs, visitable.declared_type.to_string(), false);
    }
}
