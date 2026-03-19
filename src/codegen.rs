use core::panic;
use std::{collections::HashMap, path::Path};

use inkwell::{
    OptimizationLevel,
    builder::Builder,
    context::Context,
    module::Module,
    targets::{Target, TargetMachine},
    types::BasicType,
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, FloatValue, FunctionValue, GlobalValue,
        IntValue, PointerValue,
    },
};

use crate::{
    ast::{Declaration, Expression, FuncDecl, IfStmt, LetStmt, Program, Stmt, VarType},
    tokenizer::Operator,
};
use thiserror::Error;

/*
* Might have errors in here be all panics
* If I encounter an invalid ast in here it means I didnt properly do the Semantic Analysis
* If theres an error produced by inkwell then theres probably something wrong with the way I set it up
* Philosophy: if Programmer Error then panic, elif User Error then thiserror
*/

//CG Error is an internal error enum for backtracking on failure
// not the best option but the alternative is an ast redesign which i dont wanna do b/c lazy
#[derive(Debug, Error, PartialEq, Eq)]
enum CGError {
    #[error("{0}")]
    CustomError(String),
    #[error("")]
    InvalidExpressionTypes,
}

//shoutout: https://createlang.rs/03_secondlang/codegen.html
pub struct CodeGen<'ctx> {
    //workspace for llvm
    context: &'ctx Context,

    //compilation unit / file
    module: Module<'ctx>,

    //tool to create IR instructions within a basic block
    builder: Builder<'ctx>,

    // Map from variable names to their stack allocations
    // should clear when entering new functions
    variables: HashMap<String, PointerValue<'ctx>>,

    //global variables
    globals: HashMap<String, GlobalValue<'ctx>>,

    /// Map from function names to LLVM functions
    functions: HashMap<String, FunctionValue<'ctx>>,

    current_fn: Option<(FunctionValue<'ctx>, VarType)>,
    current_return: Option<BasicValueEnum<'ctx>>,
}
//ASSUMPTION: the program has been verified to be semantically sound
impl<'ctx> CodeGen<'ctx> {
    pub fn new(ctx: &'ctx Context) -> Self {
        let module = ctx.create_module("main");
        let builder = ctx.create_builder();
        CodeGen {
            context: ctx,
            module,
            builder,
            variables: HashMap::new(),
            globals: HashMap::new(),
            functions: HashMap::new(),
            current_fn: None,
            current_return: None,
        }
    }
    //wth is going on here lifetime wise???
    fn llvm_type(&self, ty: &VarType) -> inkwell::types::BasicTypeEnum<'ctx> {
        match ty {
            VarType::Float => self.context.f32_type().into(),
            VarType::Integer => self.context.i32_type().into(),
            VarType::Bool => self.context.bool_type().into(), //handle invalid types here
            t => panic!("can not convert type: {t:?} to inkwell type"),
        }
    }
    //taking ownership of AST b/c assuming no more use for it
    //should definitely simplify ownership
    pub fn compile_program(&mut self, program: Program, show_llvm: bool, show_asm: bool) {
        Target::initialize_native(&Default::default()).expect("failed to initialize native target");
        for decl in program.declarations {
            match decl {
                Declaration::Func(func_decl) => {
                    let return_type = self.llvm_type(&func_decl.decl_return_type);

                    let mut field_types = Vec::new();
                    for param in &func_decl.field_list {
                        field_types.push(self.llvm_type(&param.field_type).into());
                    }

                    let func_type = return_type.fn_type(&field_types, false);
                    let func_val = self.module.add_function(&func_decl.name, func_type, None);

                    for (i, param) in func_decl.field_list.iter().enumerate() {
                        func_val
                            .get_nth_param(i as u32)
                            .expect("failed to get parameter")
                            .set_name(&param.name);
                    }

                    self.current_fn = Some((func_val, func_decl.decl_return_type));
                    self.functions.insert(func_decl.name.clone(), func_val);

                    self.codegen_func_body(&func_decl);
                }
                Declaration::Var(var_decl) => {
                    let ty = &var_decl.declared_type;
                    if (*ty == VarType::Integer) || (*ty == VarType::Bool) {
                        self.compile_vardecl(&var_decl)
                            .expect("invalid variable declaration");
                    } else if *ty == VarType::Float {
                        todo!("make compile_var_decl_float");
                    }
                }
            }
        }
        self.module.verify().expect("module verification failed");
        if show_llvm {
            self.module
                .print_to_file("ibrida.ll")
                .expect("failed to print to file");
        }
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::Default,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .expect("failed to build target machine");

        if show_asm {
            target_machine
                .write_to_file(
                    &self.module,
                    inkwell::targets::FileType::Assembly,
                    Path::new("ibrida.s"),
                )
                .expect("failed to write assembly file");
        }
        let output_path = Path::new("output.out");
        target_machine
            .write_to_file(
                &self.module,
                inkwell::targets::FileType::Object,
                output_path,
            )
            .expect("failed to write to object file");
    }
    //just working w/ ints at the moment. pretending like other types dont exist
    fn codegen_func_body(
        &mut self,
        FuncDecl {
            name,
            field_list,
            body,
            decl_return_type: _,
        }: &FuncDecl,
    ) {
        let func = self
            .functions
            .get(name)
            .cloned()
            .unwrap_or_else(|| panic!("function {} not declared", name));

        let entry = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry);

        for (i, p) in field_list.iter().enumerate() {
            let param_val = func.get_nth_param(i as u32).unwrap();

            let alloca = self.create_entry_block_alloca(&func, &p.name, &p.field_type);
            self.builder.build_store(alloca, param_val).unwrap();
            self.variables.insert(p.name.clone(), alloca);
        }

        // FIXME: Allow for statements that result in floats??
        // do statements even need to result in anything??

        for s in &body.inner {
            self.compile_stmt(s).expect("failed to compile statment");
        }

        if self
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            if let Some(val) = self.current_return {
                self.builder.build_return(Some(&val)).unwrap();
            } else {
                let zero = self.context.i64_type().const_int(0, false);
                self.builder.build_return(Some(&zero)).unwrap();
            }
        }
        self.current_return = None;
    }
    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<(), CGError> {
        match stmt {
            Stmt::Return(return_stmt) => {
                let return_val: BasicValueEnum = if self.current_fn.unwrap().1 == VarType::Integer
                    || self.current_fn.unwrap().1 == VarType::Bool
                {
                    self.compile_expr_int(&return_stmt.expression)?.into()
                } else {
                    self.compile_expr_float(&return_stmt.expression)?.into()
                };

                self.builder
                    .build_return(Some(&return_val))
                    .expect("failed to build return");
                self.current_return = Some(return_val);
            }
            Stmt::VarDecl(let_stmt) => {
                self.compile_vardecl(let_stmt)?;
            }
            Stmt::VarAssign(assign_stmt) => {
                let rhs_val: BasicValueEnum = if let Some(ty) = assign_stmt.checked_expr_type {
                    match ty {
                        VarType::Bool | VarType::Integer => {
                            self.compile_expr_int(&assign_stmt.rhs)?.into()
                        }
                        VarType::Float => self.compile_expr_float(&assign_stmt.rhs)?.into(),
                        _ => panic!("unexpected unknown type"),
                    }
                } else {
                    panic!("assign statement rhs type was None");
                };

                let ptr_val = self
                    .variables
                    .get(&assign_stmt.lhs)
                    .expect("could not find variable for assignment");

                self.builder
                    .build_store(*ptr_val, rhs_val)
                    .expect("failed to build store");
            }
            Stmt::IfStmt(if_stmt) => {
                self.compile_if_stmt(if_stmt)?;
            }
            Stmt::Else(_) => {}
            Stmt::FnCall(func_call) => todo!(),
        };
        Ok(())
    }
    //Produces values for expressions that result in integers
    fn compile_expr_int(&mut self, expr: &Expression) -> Result<IntValue<'ctx>, CGError> {
        match expr {
            Expression::BinaryExpr { op, lhs, rhs, ty } => {
                if !op.is_binary() {
                    panic!(
                        "Code Generation: found non-binary operator: {op:?} when expected otherwise"
                    );
                }

                if !((*ty == VarType::Integer) || (*ty == VarType::Bool)) {
                    return Err(CGError::InvalidExpressionTypes);
                }

                let l = match lhs.get_expr_type() {
                    VarType::Bool | VarType::Integer => self.compile_expr_int(lhs)?,
                    VarType::Float => {
                        let l = self.compile_expr_float(lhs)?;
                        let r = self.compile_expr_float(rhs)?;

                        let val = match op {
                            Operator::BoolEq => self
                                .builder
                                .build_float_compare(inkwell::FloatPredicate::OEQ, l, r, "eq")
                                .expect("think of a better expect msg"),
                            Operator::Less => self
                                .builder
                                .build_float_compare(inkwell::FloatPredicate::OLT, l, r, "lt")
                                .expect("think of a better expect msg"),
                            Operator::LessEq => self
                                .builder
                                .build_float_compare(inkwell::FloatPredicate::OLE, l, r, "le")
                                .expect("think of a better expect msg"),
                            Operator::Greater => self
                                .builder
                                .build_float_compare(inkwell::FloatPredicate::OGT, l, r, "gt")
                                .expect("think of a better expect msg"),
                            Operator::GreaterEq => self
                                .builder
                                .build_float_compare(inkwell::FloatPredicate::OGE, l, r, "ge")
                                .expect("think of a better expect msg"),
                            Operator::NotEq => self
                                .builder
                                .build_float_compare(inkwell::FloatPredicate::ONE, l, r, "le")
                                .expect("think of a better expect msg"),
                            o => panic!("expected float comparison, got {o:?}"),
                        };
                        return Ok(val);
                    }
                    _ => panic!("unexpected unknown vartype"),
                };
                let r = self.compile_expr_int(rhs)?;

                let val = match op {
                    Operator::Addition => self
                        .builder
                        .build_int_add(l, r, "add")
                        .expect("think of a better expect msg"),
                    Operator::Subtraction => self
                        .builder
                        .build_int_sub(l, r, "sub")
                        .expect("think of a better expect msg"),
                    Operator::Division => self
                        .builder
                        .build_int_signed_div(l, r, "div")
                        .expect("think of a better expect msg"),
                    Operator::Multiplication => self
                        .builder
                        .build_int_mul(l, r, "mul")
                        .expect("think of a better expect msg"),
                    Operator::Modulo => self
                        .builder
                        .build_int_signed_rem(l, r, "mod")
                        .expect("think of a better expect msg"),

                    Operator::BoolEq => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::EQ, l, r, "eq")
                        .expect("think of a better expect msg"),
                    Operator::Less => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::SLT, l, r, "lt")
                        .expect("think of a better expect msg"),
                    Operator::LessEq => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::SLE, l, r, "le")
                        .expect("think of a better expect msg"),
                    Operator::Greater => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::SGT, l, r, "gt")
                        .expect("think of a better expect msg"),
                    Operator::GreaterEq => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::SGE, l, r, "ge")
                        .expect("think of a better expect msg"),
                    Operator::NotEq => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::NE, l, r, "le")
                        .expect("think of a better expect msg"),
                    o => panic!("operator {o:?} can not be used here"),
                };
                Ok(val)
            }
            Expression::Literal(lit, lit_type) => match lit_type {
                VarType::Integer => {
                    let literal = lit.parse::<i32>().unwrap();
                    Ok(self.context.i32_type().const_int(literal as u64, true))
                }
                VarType::Bool => {
                    let literal = lit.parse::<bool>().unwrap();
                    Ok(self.context.bool_type().const_int(literal as u64, false))
                }
                t => panic!("expected integer or bool, got {t}"),
            },
            Expression::UnaryExpr { op, operand, ty: _ } => {
                if !op.is_unary() {
                    panic!("got operator {op:?} when expected unary operator");
                }
                let val = self.compile_expr_int(operand)?;
                match op {
                    Operator::BoolNot => Ok(self
                        .builder
                        .build_not(val, "not")
                        .expect("failed to produce int value for NOT expression")),
                    _ => unreachable!("if got here, update code for additional unary operators"),
                }
            }
            Expression::Var(var_name, _) => {
                let var_ptr = self
                    .variables
                    .get(var_name)
                    .expect("failed to find variable in hashmap");

                let val = self
                    .builder
                    .build_load(self.context.i32_type(), *var_ptr, var_name)
                    .expect("failed to build load instruction");

                Ok(val.into_int_value())
            }
            Expression::FuncCall(func_call, _) => {
                let func_val = self
                    .functions
                    .get(&func_call.id)
                    .cloned()
                    .expect("failed to find function call in hashmap");

                let arg_values: Vec<BasicMetadataValueEnum> = func_call
                    .args
                    .iter()
                    .map(|a| match a.get_expr_type() {
                        VarType::Bool | VarType::Integer => {
                            self.compile_expr_int(a).map(|v| v.into())
                        }
                        VarType::Float => self.compile_expr_float(a).map(|v| v.into()),
                        _ => panic!("unknown expression type unexpected"),
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let call = self
                    .builder
                    .build_call(func_val, &arg_values, "call")
                    .expect("failed to build function call");

                Ok(call.try_as_basic_value().unwrap_basic().into_int_value())
            }
        }
    }
    fn compile_expr_float(&mut self, expr: &Expression) -> Result<FloatValue<'ctx>, CGError> {
        match expr {
            Expression::BinaryExpr { op, lhs, rhs, ty } => {
                if !op.is_binary() || op.is_comparison() {
                    panic!(
                        "Code Generation: found non-binary operator: {op:?} when expected otherwise"
                    );
                }
                if *ty != VarType::Float {
                    return Err(CGError::InvalidExpressionTypes);
                }

                let l = self.compile_expr_float(lhs)?;
                let r = self.compile_expr_float(rhs)?;

                let val = match op {
                    Operator::Addition => self
                        .builder
                        .build_float_add(l, r, "add")
                        .expect("think of a better expect msg"),
                    Operator::Subtraction => self
                        .builder
                        .build_float_sub(l, r, "sub")
                        .expect("think of a better expect msg"),
                    Operator::Division => self
                        .builder
                        .build_float_div(l, r, "div")
                        .expect("think of a better expect msg"),
                    Operator::Multiplication => self
                        .builder
                        .build_float_mul(l, r, "mul")
                        .expect("think of a better expect msg"),

                    o => panic!("operator {o:?} can not be used here"),
                };

                Ok(val)
            }
            Expression::Literal(lit, lit_type) => {
                let literal = match lit_type {
                    VarType::Float => lit.parse::<f32>().unwrap(),
                    t => panic!("expected float, got {t}"),
                };
                Ok(self.context.f32_type().const_float(literal as f64))
            }
            Expression::UnaryExpr {
                op,
                operand: _,
                ty: _,
            } => {
                if !op.is_unary() {
                    panic!("got operator {op:?} when expected unary operator");
                }
                panic!("Unary Expressions are disallowed for floats");
            }
            Expression::Var(var_name, _) => {
                let var_ptr = self
                    .variables
                    .get(var_name)
                    .expect("failed to find variable in hashmap");

                let val = self
                    .builder
                    .build_load(self.context.f32_type(), *var_ptr, var_name)
                    .expect("failed to build load instruction");

                Ok(val.into_float_value())
            }
            Expression::FuncCall(func_call, _) => {
                let func_val = self
                    .functions
                    .get(&func_call.id)
                    .cloned()
                    .expect("failed to find function call in hashmap");

                let arg_values: Vec<BasicMetadataValueEnum> = func_call
                    .args
                    .iter()
                    .map(|a| match a.get_expr_type() {
                        VarType::Bool | VarType::Integer => {
                            self.compile_expr_int(a).map(|v| v.into())
                        }
                        VarType::Float => self.compile_expr_float(a).map(|v| v.into()),
                        _ => panic!("unknown expression type unexpected"),
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let call = self
                    .builder
                    .build_call(func_val, &arg_values, "call")
                    .expect("failed to build function call");

                Ok(call.try_as_basic_value().unwrap_basic().into_float_value())
            }
        }
    }
    fn compile_vardecl(&mut self, let_stmt: &LetStmt) -> Result<(), CGError> {
        let rhs_val: BasicValueEnum = if let_stmt.declared_type == VarType::Integer
            || let_stmt.declared_type == VarType::Bool
        {
            self.compile_expr_int(&let_stmt.rhs)?.into()
        } else {
            self.compile_expr_float(&let_stmt.rhs)?.into()
        };
        let ty = self.llvm_type(&let_stmt.declared_type);
        let ptr_val = self
            .builder
            .build_alloca(ty, &let_stmt.lhs)
            .expect("failed building allocation");

        self.builder
            .build_store(ptr_val, rhs_val)
            .expect("failed to build store");
        self.variables.insert(let_stmt.lhs.clone(), ptr_val);
        Ok(())
    }
    fn create_entry_block_alloca(
        &self,
        function: &FunctionValue<'ctx>,
        name: &str,
        ty: &VarType,
    ) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = function.get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(inst) => builder.position_before(&inst),
            None => builder.position_at_end(entry),
        }

        let llvm_type = self.llvm_type(ty);
        builder.build_alloca(llvm_type, name).unwrap()
    }
    fn compile_if_stmt(&mut self, if_stmt: &IfStmt) -> Result<(), CGError> {
        let expr_val = self.compile_expr_int(&if_stmt.condition)?;
        let expr_bool = self
            .builder
            .build_int_truncate(expr_val, self.context.bool_type(), "bool")
            .expect("failed to truncate int to bool");

        let func = self.current_fn.unwrap().0;
        let then_block = self.context.append_basic_block(func, "then");
        let merge_block = self.context.append_basic_block(func, "merge");

        let else_block = if if_stmt.else_stmt.is_some() {
            self.context.append_basic_block(func, "else")
        } else {
            merge_block
        };

        self.builder
            .build_conditional_branch(expr_bool, then_block, else_block)
            .unwrap();

        self.builder.position_at_end(then_block);
        for stmt in &if_stmt.body.inner {
            self.compile_stmt(stmt)?;
        }

        let then_end = self.builder.get_insert_block().unwrap();
        let then_has_term = then_end.get_terminator().is_some();
        if !then_has_term {
            self.builder
                .build_unconditional_branch(merge_block)
                .unwrap();
        }

        if if_stmt.else_stmt.is_some() {
            self.builder.position_at_end(else_block);

            for stmt in &if_stmt.else_stmt.as_ref().unwrap().body.inner {
                self.compile_stmt(stmt)?;
            }

            let else_end = self.builder.get_insert_block().unwrap();
            let else_has_term = else_end.get_terminator().is_some();
            if !else_has_term {
                self.builder
                    .build_unconditional_branch(merge_block)
                    .unwrap();
            }
            if then_has_term && else_has_term {
                unsafe {
                    merge_block.delete().unwrap();
                }
                return Ok(());
            }
            // if i ever decide to add stuff like: let foo: i32 = if (bar) {5} else{7}
            else {
                self.builder.position_at_end(merge_block);

                return Ok(());
            }
        } else {
            // No else statement: position at merge_block for subsequent statements
            self.builder.position_at_end(merge_block);
        }
        Ok(())
    }
}
