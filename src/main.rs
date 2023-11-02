pub mod args;
pub mod compiler;
pub mod interpreter;
pub mod lexer;
pub mod parser;

use args::LuArgs;
use clap::Parser;
use interpreter::{FunctionKind, RunTimeError, Value};
use oneparse::{parse, position::Located};
use std::{collections::HashMap, fs, process::exit};

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
    // dbg!(&main_closure);
    match Interpreter::default()
        .with_globals(std_env())
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

pub fn std_env() -> HashMap<String, Value> {
    let mut env = HashMap::new();
    env.insert(
        "__module".into(),
        Value::Null,
    );
    env.insert(
        "print".into(),
        Value::Function(FunctionKind::NativeFunction(_print)),
    );
    env
}
pub fn _print(
    _: &mut Interpreter,
    args: Vec<Value>,
) -> Result<Option<Value>, Located<RunTimeError>> {
    for arg in args {
        print!("{arg}");
    }
    println!();
    Ok(None)
}
pub fn _abs(_: &mut Interpreter, mut args: Vec<Value>) -> Result<Option<Value>, Located<RunTimeError>> {
    if args.is_empty() {
        return Ok(None);
    }
    let value = args.remove(0);
    match value {
        Value::Int(value) => Ok(Some(Value::Int(value.abs()))),
        Value::Float(value) => Ok(Some(Value::Float(value.abs()))),
        _ => Ok(None)
    }
}
pub fn _time(_: &mut Interpreter, _: Vec<Value>) -> Result<Option<Value>, Located<RunTimeError>> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let start = SystemTime::now();
    let since_the_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards");
    Ok(Some(Value::Int(since_the_epoch.as_secs() as i32)))
}
