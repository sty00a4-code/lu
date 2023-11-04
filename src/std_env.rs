use std::{collections::HashMap, rc::Rc, cell::RefCell};

use oneparse::position::{Located, Positon};

use crate::interpreter::{FunctionKind, Interpreter, RunTimeError, Value, Object};

macro_rules! make_module {
    ($name:literal : $($var:literal = $value:expr),*) => {
        {
            let mut module = Object {
                meta: {
                    let mut meta = Object::default();
                    meta.map.insert("__name".to_string(), Value::String($name.to_string()));
                    Some(Box::new(Rc::new(RefCell::new(meta))))
                },
                ..Default::default()
            };
            $(
                module.map.insert($var.to_string(), $value);
            )*
            Value::Object(Rc::new(RefCell::new(module)))
        }
    };
}

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
    env.insert("math".to_string(), make_module!("math":
        "abs" = Value::Function(FunctionKind::NativeFunction(_math_abs))
    ));
    env.insert("vec".to_string(), make_module!("math":
        "len" = Value::Function(FunctionKind::NativeFunction(_vec_len)),
        "push" = Value::Function(FunctionKind::NativeFunction(_vec_push)),
        "insert" = Value::Function(FunctionKind::NativeFunction(_vec_insert)),
        "pop" = Value::Function(FunctionKind::NativeFunction(_vec_pop)),
        "remove" = Value::Function(FunctionKind::NativeFunction(_vec_remove)),
        "pos" = Value::Function(FunctionKind::NativeFunction(_vec_pos))
    ));
    
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
    if let Value::Object(object) = object {
        if let Some(Value::Object(meta)) = meta {
            object.borrow_mut().meta = Some(Box::new(meta));
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
        Ok(object.borrow().meta.clone().map(|meta| *meta).map(Value::Object))
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
    Err(Located::new(
        RunTimeError::Custom(args.remove(0).to_string()),
        pos.clone(),
    ))
}
pub fn _exit(
    _: &mut Interpreter,
    args: Vec<Value>,
    _: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    let code = args.get(0).cloned().unwrap_or(Value::Int(0));
    std::process::exit(if let Value::Int(code) = code { code } else { 0 });
}

pub fn _vec_len(
    _: &mut Interpreter,
    mut args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    if args.is_empty() {
        return Ok(None)
    }
    Ok(Some(Value::Int(match args.remove(0) {
        Value::Vector(vector) => vector.borrow().len() as i32,
        value => return Err(Located::new(RunTimeError::Custom(format!("expected {}, got {}", Value::Vector(Rc::default()).typ(), value.typ())), pos.clone()))
    })))
}
pub fn _vec_push(
    _: &mut Interpreter,
    mut args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    if args.is_empty() {
        return Ok(None)
    }
    match args.remove(0) {
        Value::Vector(vector) => vector.borrow_mut().push(args.get(0).cloned().unwrap_or_default()),
        value => return Err(Located::new(RunTimeError::Custom(format!("expected {} for argument #1, got {}", Value::Vector(Rc::default()).typ(), value.typ())), pos.clone()))
    }
    Ok(None)
}
pub fn _vec_insert(
    _: &mut Interpreter,
    mut args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    if args.is_empty() {
        return Ok(None)
    }
    if args.len() < 3 {
        return Ok(None)
    }
    let vector = args.remove(0);
    let index = args.remove(0);
    let value = args.remove(0);
    if let Value::Vector(vector) = &vector {
        if let Value::Int(index) = index {
            let index = index.unsigned_abs() as usize;
            vector.borrow_mut().insert(index, value);
            return Ok(None)
        }
    }
    Err(Located::new(RunTimeError::Custom(format!("expected {} for argument #1, got {}", Value::Vector(Rc::default()).typ(), vector.typ())), pos.clone()))
}
pub fn _vec_pop(
    _: &mut Interpreter,
    mut args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    if args.is_empty() {
        return Ok(None)
    }
    let vector = args.remove(0);
    if let Value::Vector(vector) = vector {
        Ok(vector.borrow_mut().pop())
    } else {
        Err(Located::new(RunTimeError::Custom(format!("expected {} for argument #1, got {}", Value::Vector(Rc::default()).typ(), vector.typ())), pos.clone()))
    }
}
pub fn _vec_remove(
    _: &mut Interpreter,
    mut args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    if args.is_empty() {
        return Ok(None)
    }
    if args.len() < 2 {
        return Ok(None)
    }
    let vector = args.remove(0);
    let index = args.remove(0);
    if let Value::Vector(vector) = &vector {
        if let Value::Int(index) = index {
            let index = index.unsigned_abs() as usize;
            if vector.borrow().get(index).is_some() {
                return Ok(Some(vector.borrow_mut().remove(0)))
            } else {
                return Ok(None)
            }
        }
    }
    Err(Located::new(RunTimeError::Custom(format!("expected {} for argument #1, got {}", Value::Vector(Rc::default()).typ(), vector.typ())), pos.clone()))
}
pub fn _vec_pos(
    _: &mut Interpreter,
    mut args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    if args.is_empty() {
        return Ok(None)
    }
    if args.len() < 2 {
        return Ok(None)
    }
    let vector = args.remove(0);
    let value = args.remove(0);
    if let Value::Vector(vector) = &vector {
        Ok(vector.borrow().iter().position(|e| e == &value).map(|index| Value::Int(index as i32)))
    } else {
        Err(Located::new(RunTimeError::Custom(format!("expected {} for argument #1, got {}", Value::Vector(Rc::default()).typ(), vector.typ())), pos.clone()))
    }
}

pub fn _math_abs(
    _: &mut Interpreter,
    mut args: Vec<Value>,
    _: &Positon,
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
    _: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let start = SystemTime::now();
    let since_the_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards");
    Ok(Some(Value::Int(since_the_epoch.as_secs() as i32)))
}
