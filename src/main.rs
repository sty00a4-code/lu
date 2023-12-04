pub mod compiler;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod std_env;

use compiler::Closure;
use interpreter::{PathLocated, Traced};
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
pub fn compile_ast(ast: Located<Chunk>, path: &str) -> Result<Closure, Located<CompileError>> {
    let mut compiler = Compiler::new(path.to_string());
    ast.compile(&mut compiler)?;
    Ok(compiler.closures.pop().unwrap())
}

fn main() {
    let Some(path) = std::env::args().nth(1) else {
        return;
    };
    let text = match fs::read_to_string(&path) {
        Ok(text) => text,
        Err(err) => {
            eprintln!("ERROR: {err}");
            exit(1);
        }
    };
    let main_closure = match generate_ast(text) {
        Ok(ast) => match compile_ast(ast, &path) {
            Ok(closure) => closure,
            Err(Located { value: err, pos }) => {
                eprintln!(
                    "ERROR {}:{}:{}: {err}",
                    &path,
                    pos.ln.start + 1,
                    pos.col.start + 1,
                );
                exit(1);
            }
        },
        Err(Located { value: err, pos }) => {
            eprintln!(
                "ERROR {}:{}:{}: {err}",
                &path,
                pos.ln.start + 1,
                pos.col.start + 1,
            );
            exit(1);
        }
    };
    println!("{}", &main_closure);
    match Interpreter::default()
        .with_globals(std_env::std_env())
        .run(Rc::new(RefCell::new(main_closure)))
    {
        Ok(value) => {
            if let Some(value) = value {
                println!("{value}");
            }
        }
        Err(Traced {
            trace,
            err:
                PathLocated {
                    located: Located { value: err, pos },
                    path,
                },
        }) => {
            eprintln!(
                "ERROR {}:{}:{}: {err}",
                &path,
                pos.ln.start + 1,
                pos.col.start + 1
            );
            if !trace.is_empty() {
                eprintln!("trace back (last call first)");
                for (path, pos) in trace {
                    eprintln!("\t{path}:{}:{}", pos.ln.start + 1, pos.col.start + 1);
                }
            }
            exit(1);
        }
    }
}
