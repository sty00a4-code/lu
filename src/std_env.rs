use std::{collections::HashMap, rc::Rc, cell::RefCell};

use oneparse::position::{Located, Positon};

use crate::interpreter::{FunctionKind, Interpreter, RunTimeError, Value, Object};

macro_rules! make_module {
    ($name:literal : $($var:literal = $value:expr),*) => {
        {
            #[allow(unused_mut)]
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
macro_rules! collect_args {
    ($args:ident $pos:ident : $($var:pat => if ! $value:expr),* => $body:block) => {
        {
            let mut arg_idx = 0;
            $(
                arg_idx += 1;
                #[allow(irrefutable_let_patterns)]
                let $var = $args.get(arg_idx - 1).cloned().unwrap_or_default() else {
                    return Err(Located::new(RunTimeError::Custom(format!("expected {} for argument #{}, got {}", $value.typ(), arg_idx - 1, $args.get(arg_idx - 1).cloned().unwrap_or_default().typ())), $pos.clone()))
                };
            )*
            $body
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
        "floor" = Value::Function(FunctionKind::NativeFunction(_math_floor)),
        "ceil" = Value::Function(FunctionKind::NativeFunction(_math_ceil)),
        "round" = Value::Function(FunctionKind::NativeFunction(_math_round)),
        "abs" = Value::Function(FunctionKind::NativeFunction(_math_abs)),
        "cos" = Value::Function(FunctionKind::NativeFunction(_math_cos)),
        "sin" = Value::Function(FunctionKind::NativeFunction(_math_sin)),
        "tan" = Value::Function(FunctionKind::NativeFunction(_math_tan)),
        "acos" = Value::Function(FunctionKind::NativeFunction(_math_acos)),
        "asin" = Value::Function(FunctionKind::NativeFunction(_math_asin)),
        "atan" = Value::Function(FunctionKind::NativeFunction(_math_atan)),
        "acosh" = Value::Function(FunctionKind::NativeFunction(_math_acosh)),
        "asinh" = Value::Function(FunctionKind::NativeFunction(_math_asinh)),
        "atanh" = Value::Function(FunctionKind::NativeFunction(_math_atanh)),
        "atan2" = Value::Function(FunctionKind::NativeFunction(_math_atan2))
    ));
    env.insert("vec".to_string(), make_module!("vec":
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
    println!("{}", args.into_iter().map(|value| value.to_string()).collect::<Vec<String>>().join("\t"));
    Ok(None)
}
pub fn _setmeta(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        Value::Object(object) => if ! Value::Object(Default::default()),
        meta => if ! Value::default()
        => {
            if let Value::Object(meta) = meta {
                object.borrow_mut().meta = Some(Box::new(meta));
            }
            Ok(Some(Value::Object(object)))
        }
    )
}
pub fn _getmeta(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        Value::Object(object) => if ! Value::Object(Default::default())
        => {
            let object = object.borrow();
            Ok(object.meta.clone().map(|meta| *meta).map(Value::Object))
        }
    )
}
pub fn _error(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        Value::String(msg) => if ! Value::Int(Default::default())
        => {
            Err(Located::new(
                RunTimeError::Custom(msg),
                pos.clone(),
            ))
        }
    )
}
pub fn _exit(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        Value::Int(code) => if ! Value::Int(Default::default())
        => {
            std::process::exit(code);
        }
    )
}

pub fn _vec_len(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        Value::Vector(vector) => if ! Value::Vector(Default::default())
        => {
            let vector = vector.borrow();
            Ok(Some(Value::Int(vector.len() as i32)))
        }
    )
}
pub fn _vec_push(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        Value::Vector(vector) => if ! Value::Vector(Default::default()),
        value => if ! Value::default()
        => {
            vector.borrow_mut().push(value);
            Ok(None)
        }
    )
}
pub fn _vec_insert(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        Value::Vector(vector) => if ! Value::Vector(Default::default()),
        Value::Int(index) => if ! Value::Vector(Default::default()),
        value => if ! Value::default()
        => {
            let index = index.unsigned_abs() as usize;
            vector.borrow_mut().insert(index, value);
            Ok(None)
        }
    )
}
pub fn _vec_pop(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        Value::Vector(vector) => if ! Value::Vector(Default::default())
        => {
            let mut vector = vector.borrow_mut();
            Ok(vector.pop())
        }
    )
}
pub fn _vec_remove(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        Value::Vector(vector) => if ! Value::Vector(Default::default()),
        Value::Int(index) => if ! Value::Vector(Default::default())
        => {
            let index = index.unsigned_abs() as usize;
            if vector.borrow().get(index).is_some() {
                return Ok(Some(vector.borrow_mut().remove(0)))
            }
            Ok(None)
        }
    )
}
pub fn _vec_pos(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        Value::Vector(vector) => if ! Value::Vector(Default::default()),
        value => if ! Value::default()
        => {
            let vector = vector.borrow();
            Ok(vector.iter().position(|e| e == &value).map(|index| Value::Int(index as i32)))
        }
    )
}

pub fn _math_floor(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        value => if ! Value::Float(Default::default())
        => {
            match value {
                Value::Int(value) => Ok(Some(Value::Int(value))),
                Value::Float(value) => Ok(Some(Value::Float(value.floor()))),
                value => Err(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()))
            }
        }
    )
}
pub fn _math_ceil(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        value => if ! Value::Float(Default::default())
        => {
            match value {
                Value::Int(value) => Ok(Some(Value::Int(value))),
                Value::Float(value) => Ok(Some(Value::Float(value.ceil()))),
                value => Err(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()))
            }
        }
    )
}
pub fn _math_round(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        value => if ! Value::Float(Default::default())
        => {
            match value {
                Value::Int(value) => Ok(Some(Value::Int(value))),
                Value::Float(value) => Ok(Some(Value::Float(value.round()))),
                value => Err(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()))
            }
        }
    )
}
pub fn _math_abs(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        value => if ! Value::Float(Default::default())
        => {
            match value {
                Value::Int(value) => Ok(Some(Value::Int(value.abs()))),
                Value::Float(value) => Ok(Some(Value::Float(value.abs()))),
                value => Err(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()))
            }
        }
    )
}
pub fn _math_cos(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f32,
                Value::Float(value) => value,
                value => return Err(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()))
            };
            Ok(Some(Value::Float(value.cos())))
        }
    )
}
pub fn _math_sin(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f32,
                Value::Float(value) => value,
                value => return Err(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()))
            };
            Ok(Some(Value::Float(value.sin())))
        }
    )
}
pub fn _math_tan(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f32,
                Value::Float(value) => value,
                value => return Err(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()))
            };
            Ok(Some(Value::Float(value.tan())))
        }
    )
}
pub fn _math_acos(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f32,
                Value::Float(value) => value,
                value => return Err(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()))
            };
            Ok(Some(Value::Float(value.acos())))
        }
    )
}
pub fn _math_asin(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f32,
                Value::Float(value) => value,
                value => return Err(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()))
            };
            Ok(Some(Value::Float(value.asin())))
        }
    )
}
pub fn _math_atan(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f32,
                Value::Float(value) => value,
                value => return Err(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()))
            };
            Ok(Some(Value::Float(value.atan())))
        }
    )
}
pub fn _math_cosh(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f32,
                Value::Float(value) => value,
                value => return Err(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()))
            };
            Ok(Some(Value::Float(value.cosh())))
        }
    )
}
pub fn _math_sinh(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f32,
                Value::Float(value) => value,
                value => return Err(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()))
            };
            Ok(Some(Value::Float(value.sinh())))
        }
    )
}
pub fn _math_tanh(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f32,
                Value::Float(value) => value,
                value => return Err(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()))
            };
            Ok(Some(Value::Float(value.tanh())))
        }
    )
}
pub fn _math_acosh(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f32,
                Value::Float(value) => value,
                value => return Err(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()))
            };
            Ok(Some(Value::Float(value.acosh())))
        }
    )
}
pub fn _math_asinh(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f32,
                Value::Float(value) => value,
                value => return Err(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()))
            };
            Ok(Some(Value::Float(value.asinh())))
        }
    )
}
pub fn _math_atanh(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f32,
                Value::Float(value) => value,
                value => return Err(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()))
            };
            Ok(Some(Value::Float(value.atanh())))
        }
    )
}
pub fn _math_atan2(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
) -> Result<Option<Value>, Located<RunTimeError>> {
    collect_args!(args pos:
        value => if ! Value::Float(Default::default()),
        value2 => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f32,
                Value::Float(value) => value,
                value => return Err(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()))
            };
            let value2 = match value2 {
                Value::Int(value) => value as f32,
                Value::Float(value) => value,
                value => return Err(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()))
            };
            Ok(Some(Value::Float(value.atan2(value2))))
        }
    )
}