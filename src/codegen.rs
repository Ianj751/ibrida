/* use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicType,
    values::{FunctionValue, PointerValue},
};

use crate::{
    ast::{Declaration, FuncDecl, LetStmt, Program, VarType},
    semantic_analyzer::{SymbolTable, Visit},
};
use thiserror::Error;

/*
* Another Visitor?
Could traverse AST and build IR from there
 */
#[derive(Debug, Error)]
pub enum CodeGenError {}

//Code Generation Visitor
type CGResult = Result<(), CodeGenError>;
//shoutout: https://createlang.rs/03_secondlang/codegen.html
struct CGVisitor<'ctx> {
    //workspace for llvm
    context: &'ctx Context,
    //compilation unit / file
    module: Module<'ctx>,
    //tool to create IR instructions within a basic block
    builder: Builder<'ctx>,
    /// Map from variable names to their stack allocations
    variables: HashMap<String, PointerValue<'ctx>>,
    /// Map from function names to LLVM functions
    functions: HashMap<String, FunctionValue<'ctx>>,
    /// Current function being compiled
    current_fn: Option<FunctionValue<'ctx>>,
}
impl<'ctx> CGVisitor<'ctx> {
    //wth is going on here lifetime wise???
    fn vartype_to_llvm(&self, ty: &VarType) -> inkwell::types::BasicTypeEnum<'ctx> {
        match ty {
            VarType::Float => self.context.f32_type().into(),
            VarType::Integer => self.context.i32_type().into(),
            _ => todo!(), //handle invalid types here
        }
    }
}

//ASSUMPTION: the program has been verified to be semantically sound
impl Visit<Program, CodeGenError> for CGVisitor<'_> {
    fn visit(&mut self, visitable: &mut Program) -> CGResult {
        for decl in &mut visitable.declarations {
            self.visit(decl)?;
        }
        Ok(())
    }
}
impl Visit<Declaration, CodeGenError> for CGVisitor<'_> {
    fn visit(&mut self, visitable: &mut Declaration) -> CGResult {
        match visitable {
            Declaration::Func(func_decl) => self.visit(func_decl)?,
            Declaration::Var(var_decl) => self.visit(var_decl)?,
        };
        Ok(())
    }
}

impl Visit<FuncDecl, CodeGenError> for CGVisitor<'_> {
    fn visit(&mut self, visitable: &mut FuncDecl) -> CGResult {
        let ty = self.vartype_to_llvm(&visitable.decl_return_type);

        let mut field_types = Vec::new();
        for param in &visitable.field_list {
            field_types.push(self.vartype_to_llvm(&param.field_type).into());
        }

        let func_type = ty.fn_type(&field_types, false);
        let func_val = self.module.add_function(&visitable.name, func_type, None);
        self.current_fn = Some(func_val);

        todo!("visit body and using hashmaps gen ir instructions");
        Ok(())
    }
}
impl Visit<LetStmt, CodeGenError> for CGVisitor<'_> {
    fn visit(&mut self, visitable: &mut LetStmt) -> CGResult {
        todo!("alloc on the stack the variable and gen instr for expr");
        Ok(())
    }
}
 */
