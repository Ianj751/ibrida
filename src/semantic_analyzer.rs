use std::{collections::HashMap, iter::Scan, option};

use thiserror::Error;

use crate::ast::{AstNode, VarType};

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
    pub fn lookup_symbol(self, name: &str) -> Option<Symbol> {
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

// node: node to analyze and append typed version to root
// operates recursively on root
pub fn semantic_analyze_node(node: &AstNode, root: &mut TypedNode) {}
