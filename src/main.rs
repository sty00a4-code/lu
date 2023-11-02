pub mod args;
pub mod compiler;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod std_env;

use args::LuArgs;
use clap::Parser;
use oneparse::{parse, position::Located};
use std::{fs, process::exit};

use crate::{
    compiler::{Compilable, Compiler},
    interpreter::Interpreter,
    lexer::Token,
    parser::Chunk,
};

fn main() {
    let args = LuArgs::parse();
    let text = match fs::read_to_string(&args.path) {
        Ok(text) => text,
        Err(err) => {
            eprintln!("ERROR {err}");
            exit(1);
        }
    };
    // dbg!(&text);
    let ast = match parse::<Token, Chunk>(text) {
        Ok(ast) => ast,
        Err(Located { value: err, pos }) => {
            eprintln!(
                "ERROR {}:{}:{}: {err}",
                &args.path,
                pos.ln.start + 1,
                pos.col.start + 1
            );
            exit(1);
        }
    };
    // dbg!(&ast);
    let main_closure = {
        let mut compiler = Compiler::default();
        match ast.compile(&mut compiler) {
            Ok(()) => {}
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
        compiler.closures[0].clone()
    };
    // println!("{}", &main_closure);
    match Interpreter::default()
        .with_globals(std_env::std_env())
        .run(main_closure)
    {
        Ok(value) => if let Some(value) = value {
            println!("{value}");
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


