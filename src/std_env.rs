use std::{collections::HashMap, rc::Rc};

use oneparse::position::{Located, Positon};

use crate::interpreter::{FunctionKind, Interpreter, RunTimeError, Value};

pub fn std_env() -> HashMap<String, Value> {
    let mut env = HashMap::new();
    env.insert("__module".into(), Value::Null);
    env.insert(
        "setmeta".into(),
        Value::Function(FunctionKind::NativeFunction(_setmeta)),
    );
    env.insert(
        "getmeta".into(),
        Value::Function(FunctionKind::NativeFunction(_getmeta)),
    );
    env.insert(
        "print".into(),
        Value::Function(FunctionKind::NativeFunction(_print)),
    );
    env.insert(
        "error".into(),
        Value::Function(FunctionKind::NativeFunction(_error)),
    );
    env.insert(
        "exit".into(),
        Value::Function(FunctionKind::NativeFunction(_exit)),
    );
    env
}
pub fn _print(
    _: &mut Interpreter,
    args: Vec<Value>,
    _: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    for arg in args {
        print!("{arg}");
    }
    println!();
    Ok(None)
}
pub fn _setmeta(
    _: &mut Interpreter,
    mut args: Vec<Value>,
    _: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    if args.is_empty() {
        return Ok(None);
    }
    let object = args.remove(0);
    let meta = if args.is_empty() {
        None
    } else {
        Some(args.remove(0))
    };
    if let Value::Object(mut object) = object {
        if let Some(Value::Object(meta)) = meta {
            let object = Rc::make_mut(&mut object);
            object.meta = Some(meta);
        }
        Ok(Some(Value::Object(object)))
    } else {
        Ok(None)
    }
}
pub fn _getmeta(
    _: &mut Interpreter,
    mut args: Vec<Value>,
    _: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    if args.is_empty() {
        return Ok(None);
    }
    if let Value::Object(object) = args.remove(0) {
        Ok(object.meta.clone().map(Value::Object))
    } else {
        Ok(None)
    }
}
pub fn _error(
    _: &mut Interpreter,
    mut args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    if args.is_empty() {
        return Ok(None);
    }
    Err(Located::new(RunTimeError::Custom(args.remove(0).to_string()), pos.clone()))
}
pub fn _exit(
    _: &mut Interpreter,
    args: Vec<Value>,
    _: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    let code = args.get(0).cloned().unwrap_or(Value::Int(0));
    std::process::exit(if let Value::Int(code) = code {
        code
    } else {
        0
    });
}
pub fn _math_abs(
    _: &mut Interpreter,
    mut args: Vec<Value>,
    _: &Positon
) -> Result<Option<Value>, Located<RunTimeError>> {
    if args.is_empty() {
        return Ok(None);
    }
    let value = args.remove(0);
    match value {
        Value::Int(value) => Ok(Some(Value::Int(value.abs()))),
        Value::Float(value) => Ok(Some(Value::Float(value.abs()))),
        _ => Ok(None),
    }
}
pub fn _time_now(
    _: &mut Interpreter,
    _: Vec<Value>,
    _: &Positon
) -> Result<Option<Value>, Located<RunTimeError>> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let start = SystemTime::now();
    let since_the_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards");
    Ok(Some(Value::Int(since_the_epoch.as_secs() as i32)))
}
