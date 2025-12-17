mod parser;
mod tokenizer;

use std::{env, fs::File, io::Read};

// use crate::tokenizer::tokenizer::Tokenizer;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <path to .ibi file to compile>", &args[0]);
        return;
    }
    // if these fail, the application is completely unusable. expect is warranted
    let mut file = File::open(&args[1]).expect("failed to find file");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("failed to read file to string");

    let v: Vec<tokenizer::Token> = tokenizer::Tokenizer::new(contents).get_tokens();
    let mut iter = v.into_iter().peekable();
    let rs = parser::parse_return_stmt(&mut iter);
    dbg!(rs.unwrap());
}
