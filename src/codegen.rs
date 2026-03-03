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
        BasicMetadataValueEnum, BasicValue, FloatValue, FunctionValue, GlobalValue, IntValue,
        PointerValue,
    },
};

use crate::{
    ast::{Declaration, Expression, FuncDecl, LetStmt, Program, Stmt, VarType},
    tokenizer::Operator,
};

/*
* Might have errors in here be all panics
* If I encounter an invalid ast in here it means I didnt properly do the Semantic Analysis
* If theres an error produced by inkwell then theres probably something wrong with the way I set it up
* Philosophy: if Programmer Error then panic, elif User Error then thiserror
*/

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

    current_fn: Option<FunctionValue<'ctx>>,
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
    pub fn compile_program(&mut self, program: Program, show_llvm: bool) {
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

                    self.current_fn = Some(func_val);
                    self.functions.insert(func_decl.name.clone(), func_val);

                    self.codegen_func_body(&func_decl);
                }
                Declaration::Var(var_decl) => {
                    let _ = self.compile_vardecl(&var_decl);
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

        let output_path = Path::new("ibrida.o");
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
            let param_val = func.get_nth_param(i as u32).unwrap().into_int_value();

            let alloca = self.create_entry_block_alloca(&func, &p.name, &p.field_type);
            self.builder.build_store(alloca, param_val).unwrap();
            self.variables.insert(p.name.clone(), alloca);
        }

        let mut last_val = None;
        for s in &body.inner {
            last_val = self.compile_stmt(s);
        }

        // Add return if needed
        if self
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            if let Some(val) = last_val {
                self.builder.build_return(Some(&val)).unwrap();
            } else {
                let zero = self.context.i64_type().const_int(0, false);
                self.builder.build_return(Some(&zero)).unwrap();
            }
        }
    }
    fn compile_stmt(&mut self, stmt: &Stmt) -> Option<IntValue<'ctx>> {
        match stmt {
            Stmt::Return(return_stmt) => {
                let return_val = self.compile_expr_int(&return_stmt.expression);
                let basic_val = return_val.as_basic_value_enum();

                self.builder
                    .build_return(Some(&basic_val))
                    .expect("failed to build return");

                Some(return_val)
            }
            Stmt::VarDecl(let_stmt) => Some(self.compile_vardecl(let_stmt)),
            Stmt::VarAssign(assign_stmt) => todo!(),
            Stmt::IfStmt(if_stmt) => todo!(),
            Stmt::Else(else_stmt) => todo!(),
            Stmt::FnCall(func_call) => todo!(),
        }
    }
    //Produces values for expressions that result in integers
    fn compile_expr_int(&mut self, expr: &Expression) -> IntValue<'ctx> {
        match expr {
            Expression::BinaryExpr { op, lhs, rhs, ty } => {
                if !op.is_binary() {
                    panic!(
                        "Code Generation: found non-binary operator: {op:?} when expected otherwise"
                    );
                }
                if *ty != VarType::Integer {
                    panic!("unexpected type: got{ty:?} but expected Integer")
                }

                let l = self.compile_expr_int(lhs);
                let r = self.compile_expr_int(rhs);

                match op {
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
                }
            }
            Expression::Literal(lit, lit_type) => {
                let literal = match lit_type {
                    VarType::Integer => lit.parse::<i32>().unwrap(),
                    t => panic!("expected integer, got {t}"),
                };
                self.context.i32_type().const_int(literal as u64, true)
            }
            Expression::UnaryExpr { op, operand, ty } => {
                if !op.is_unary() {
                    panic!("got operator {op:?} when expected unary operator");
                }
                let val = self.compile_expr_int(operand);
                match op {
                    Operator::BoolNot => self
                        .builder
                        .build_not(val, "not")
                        .expect("failed to produce int value for NOT expression"),
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

                val.into_int_value()
            }
            Expression::FuncCall(func_call, _) => {
                let func_val = self
                    .functions
                    .get(&func_call.id)
                    .cloned()
                    .expect("failed to find function call in hashmap");

                //FIXME: This compile expr should not be specific to ints
                let arg_values: Vec<BasicMetadataValueEnum> = func_call
                    .args
                    .iter()
                    .map(|a| self.compile_expr_int(a).into())
                    .collect();

                let call = self
                    .builder
                    .build_call(func_val, &arg_values, "call")
                    .expect("failed to build function call");

                call.try_as_basic_value().unwrap_basic().into_int_value()
            }
        }
    }
    fn compile_vardecl(&mut self, let_stmt: &LetStmt) -> IntValue<'ctx> {
        let rhs_val = self.compile_expr_int(&let_stmt.rhs);
        let ty = self.llvm_type(&let_stmt.declared_type);
        let ptr_val = self
            .builder
            .build_alloca(ty, &let_stmt.lhs)
            .expect("failed building allocation");

        self.builder
            .build_store(ptr_val, rhs_val)
            .expect("failed to build store");
        self.variables.insert(let_stmt.lhs.clone(), ptr_val);
        rhs_val
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
}
