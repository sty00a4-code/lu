pub mod args;
pub mod compiler;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod std_env;

use args::LuArgs;
use clap::Parser;
use compiler::Closure;
use oneparse::{parse, position::Located};
use parser::CompileError;
use std::{cell::RefCell, fs, process::exit, rc::Rc};

use crate::{
    compiler::{Compilable, Compiler},
    interpreter::Interpreter,
    lexer::Token,
    parser::Chunk,
};

pub fn generate_ast(text: String) -> Result<Located<Chunk>, Located<String>> {
    parse::<Token, Chunk>(text)
}
pub fn compile_ast(ast: Located<Chunk>, path: &String) -> Result<Closure, Located<CompileError>> {
    let mut compiler = Compiler::new(path.clone());
    ast.compile(&mut compiler)?;
    Ok(compiler.closures.pop().unwrap())
}

fn main() {
    let args = LuArgs::parse();
    let text = match fs::read_to_string(&args.path) {
        Ok(text) => text,
        Err(err) => {
            eprintln!("ERROR: {err}");
            exit(1);
        }
    };
    // dbg!(&ast);
    let main_closure = match generate_ast(text) {
        Ok(ast) => match compile_ast(ast, &args.path) {
            Ok(closure) => closure,
            Err(Located { value: err, pos }) => {
                eprintln!(
                    "ERROR {}:{}:{}: {err}",
                    &args.path,
                    pos.ln.start + 1,
                    pos.col.start + 1,
                );
                exit(1);
            }
        }
        Err(Located { value: err, pos }) => {
            eprintln!(
                "ERROR {}:{}:{}: {err}",
                &args.path,
                pos.ln.start + 1,
                pos.col.start + 1,
            );
            exit(1);
        }
    };
    // println!("{}", &main_closure);
    match Interpreter::default()
        .with_globals(std_env::std_env())
        .run(Rc::new(RefCell::new(main_closure)))
    {
        Ok(value) => {
            if let Some(value) = value {
                println!("{value}");
            }
        }
        Err(Located { value: err, pos }) => {
            eprintln!(
                "ERROR {}:{}:{}: {err}",
                &args.path,
                pos.ln.start + 1,
                pos.col.start + 1
            );
            exit(1);
        }
    }
}
