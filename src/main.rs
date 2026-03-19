#[allow(dead_code)] //these fields probably wont be constructed in main
mod ast;
mod codegen;
mod parser;
mod semantic_analyzer;
mod tokenizer;

use std::{fs::File, io::Read, time::SystemTime};

use clap::Parser;
use inkwell::context::Context;

use crate::{
    codegen::CodeGen,
    parser::parse_program,
    semantic_analyzer::{SAVisitor, Visit},
    tokenizer::{Token, Tokenizer},
};

#[derive(Parser)]
#[command(name = "ibrida")]
#[command(about = "Ibrida compiler", long_about = None)]
struct Args {
    /// Path to .ibi file to compile
    path: String,

    /// Outputs LLVM IR to an ibrida.ll file
    #[arg(long)]
    emit_llvm: bool,

    /// Outputs assembly to an ibrida.s file
    #[arg(long)]
    emit_asm: bool,
}

fn main() {
    let compile_time = SystemTime::now();
    let args = Args::parse();

    let path = &args.path;
    let show_llvm = args.emit_llvm;
    let show_asm = args.emit_asm;

    // if these fail, the application is completely unusable. expect is warranted
    let mut file = File::open(path).expect("failed to find file");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("failed to read file to string");
    let err_prefix = "\x1b[38;2;255;0;0mError\x1b[0m:\n";

    let lex_time = SystemTime::now();
    let v: Vec<Token> = Tokenizer::new(contents).get_tokens();
    let mut iter = v.into_iter().peekable();
    println!("Tokenization Completed: {:?}", lex_time.elapsed().unwrap());

    let parser_time = SystemTime::now();
    let mut file_ast = match parse_program(&mut iter) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("{err_prefix}Parser: {e}");
            return;
        }
    };
    println!(
        "Semantic Analysis Completed: {:?}",
        parser_time.elapsed().unwrap()
    );

    let sa_time = SystemTime::now();
    let mut sem_analyzer = SAVisitor::new();
    if let Err(e) = sem_analyzer.visit(&mut file_ast) {
        eprintln!("{err_prefix}Semantic Analyzer: {e}");
        return;
    }
    println!(
        "Semantic Analysis Completed: {:?}",
        sa_time.elapsed().unwrap()
    );

    let cg_time = SystemTime::now();
    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    codegen.compile_program(file_ast, show_llvm, show_asm);
    println!(
        "Code Generation Completed: {:?}",
        cg_time.elapsed().unwrap()
    );

    println!("Total Time Elapsed: {:?}", compile_time.elapsed().unwrap());
}
