use std::{
    cell::RefCell, collections::HashMap, fmt::Display, fs, io::{Write, Read}, path::PathBuf, rc::Rc,
    str::FromStr,
};

use oneparse::position::{Located, Positon};

use crate::{
    compile_ast, generate_ast,
    interpreter::{
        DeepClone, ForeignData, FunctionKind, Interpreter, Object, PathLocated,
        RunTimeError, Value, FilePathRef,
    },
    parser::CompileError,
};

macro_rules! make_module {
    ($name:literal : $($var:literal = $value:expr),*) => {
        {
            #[allow(unused_mut)]
            let mut module = Object {
                meta: {
                    let mut meta = Object::default();
                    meta.map.insert("__name".to_string(), Value::String($name.to_string()));
                    Some(Rc::new(RefCell::new(meta)))
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
    ($args:ident $pos:ident $path:ident : $($var:pat => if ! $value:expr),* => $body:block) => {
        {
            #[allow(unused_variables)]
            #[allow(unused_mut)]
            let mut arg_idx = 0;
            $(
                arg_idx += 1;
                #[allow(irrefutable_let_patterns)]
                let $var = $args.get(arg_idx - 1).cloned().unwrap_or_default() else {
                    return Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected {} for argument #{}, got {}", $value.typ(), arg_idx - 1, $args.get(arg_idx - 1).cloned().unwrap_or_default().typ())), $pos.clone()), $path.to_string()))
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
        "input".into(),
        Value::Function(FunctionKind::NativeFunction(_input)),
    );
    env.insert(
        "error".into(),
        Value::Function(FunctionKind::NativeFunction(_error)),
    );
    env.insert(
        "assert".into(),
        Value::Function(FunctionKind::NativeFunction(_assert)),
    );
    env.insert(
        "exit".into(),
        Value::Function(FunctionKind::NativeFunction(_os_exit)),
    );
    env.insert(
        "ok".into(),
        Value::Function(FunctionKind::NativeFunction(_ok)),
    );
    env.insert(
        "err".into(),
        Value::Function(FunctionKind::NativeFunction(_err)),
    );

    env.insert(
        "type".into(),
        Value::Function(FunctionKind::NativeFunction(_type)),
    );
    env.insert(
        "toint".into(),
        Value::Function(FunctionKind::NativeFunction(_to_int)),
    );
    env.insert(
        "tofloat".into(),
        Value::Function(FunctionKind::NativeFunction(_to_float)),
    );
    env.insert(
        "tobool".into(),
        Value::Function(FunctionKind::NativeFunction(_to_bool)),
    );
    env.insert(
        "tostr".into(),
        Value::Function(FunctionKind::NativeFunction(_to_str)),
    );
    env.insert(
        "tovec".into(),
        Value::Function(FunctionKind::NativeFunction(_to_vec)),
    );

    env.insert(
        "obj".to_string(),
        make_module!("obj":
            "copy" = Value::Function(FunctionKind::NativeFunction(_obj_copy)),
            "deep_copy" = Value::Function(FunctionKind::NativeFunction(_obj_deep_copy)),
            "keys" = Value::Function(FunctionKind::NativeFunction(_obj_keys)),
            "values" = Value::Function(FunctionKind::NativeFunction(_obj_values)),
            "get" = Value::Function(FunctionKind::NativeFunction(_obj_get))
        ),
    );
    env.insert(
        "keys".to_string(),
        Value::Function(FunctionKind::NativeFunction(_obj_keys)),
    );
    env.insert(
        "vec".to_string(),
        make_module!("vec":
            "copy" = Value::Function(FunctionKind::NativeFunction(_vec_copy)),
            "get" = Value::Function(FunctionKind::NativeFunction(_vec_get)),
            "len" = Value::Function(FunctionKind::NativeFunction(_vec_len)),
            "push" = Value::Function(FunctionKind::NativeFunction(_vec_push)),
            "insert" = Value::Function(FunctionKind::NativeFunction(_vec_insert)),
            "pop" = Value::Function(FunctionKind::NativeFunction(_vec_pop)),
            "remove" = Value::Function(FunctionKind::NativeFunction(_vec_remove)),
            "pos" = Value::Function(FunctionKind::NativeFunction(_vec_pos)),
            "range" = Value::Function(FunctionKind::NativeFunction(_vec_range)),
            "join" = Value::Function(FunctionKind::NativeFunction(_vec_join)),
            "sort" = Value::Function(FunctionKind::NativeFunction(_vec_sort)),
            "contains" = Value::Function(FunctionKind::NativeFunction(_vec_contains))
        ),
    );
    env.insert(
        "range".to_string(),
        Value::Function(FunctionKind::NativeFunction(_vec_range)),
    );
    env.insert(
        "str".to_string(),
        make_module!("str":
            "get" = Value::Function(FunctionKind::NativeFunction(_str_get)),
            "len" = Value::Function(FunctionKind::NativeFunction(_str_len)),
            "char" = Value::Function(FunctionKind::NativeFunction(_str_char)),
            "byte" = Value::Function(FunctionKind::NativeFunction(_str_byte)),
            "upper" = Value::Function(FunctionKind::NativeFunction(_str_upper)),
            "lower" = Value::Function(FunctionKind::NativeFunction(_str_lower)),
            "split" = Value::Function(FunctionKind::NativeFunction(_str_split)),
            "is_digit" = Value::Function(FunctionKind::NativeFunction(_str_is_digit)),
            "is_radix" = Value::Function(FunctionKind::NativeFunction(_str_is_radix)),
            "is_alphabetic" = Value::Function(FunctionKind::NativeFunction(_str_is_alphabetic)),
            "is_alphanumeric" = Value::Function(FunctionKind::NativeFunction(_str_is_alphanumeric)),
            "is_whitespace" = Value::Function(FunctionKind::NativeFunction(_str_is_whitespace)),
            "is_upper" = Value::Function(FunctionKind::NativeFunction(_str_is_upper)),
            "is_lower" = Value::Function(FunctionKind::NativeFunction(_str_is_lower)),
            "is_punctuation" = Value::Function(FunctionKind::NativeFunction(_str_is_punctuation)),
            "format" = Value::Function(FunctionKind::NativeFunction(_str_format))
        ),
    );
    env.insert(
        "math".to_string(),
        make_module!("math":
            "pi" = Value::Float(std::f64::consts::PI),
            "sqrt" = Value::Function(FunctionKind::NativeFunction(_math_sqrt)),
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
        ),
    );
    env.insert(
        "os".to_string(),
        make_module!("os":
            "execute" = Value::Function(FunctionKind::NativeFunction(_os_execute)),
            "args" = Value::Function(FunctionKind::NativeFunction(_os_args)),
            "var" = Value::Function(FunctionKind::NativeFunction(_os_var)),
            "setvar" = Value::Function(FunctionKind::NativeFunction(_os_set_var)),
            "env" = Value::Function(FunctionKind::NativeFunction(_os_env)),
            "exit" = Value::Function(FunctionKind::NativeFunction(_os_exit))
        ),
    );
    env.insert(
        "fs".to_string(),
        make_module!("fs":
            "read" = Value::Function(FunctionKind::NativeFunction(_fs_read)),
            "write" = Value::Function(FunctionKind::NativeFunction(_fs_write)),
            "rename" = Value::Function(FunctionKind::NativeFunction(_fs_rename)),
            "remove" = Value::Function(FunctionKind::NativeFunction(_fs_remove)),
            "removedir" = Value::Function(FunctionKind::NativeFunction(_fs_remove_dir))
        ),
    );

    env.insert(
        "require".to_string(),
        Value::Function(FunctionKind::NativeFunction(_require)),
    );

    env.insert(
        "net".to_string(),
        make_module!("net":
            "bind" = Value::Function(FunctionKind::NativeFunction(_net_bind)),
            "connect" = Value::Function(FunctionKind::NativeFunction(_net_connect))
        ),
    );

    env
}
pub fn _print(
    _: &mut Interpreter,
    args: Vec<Value>,
    _: &Positon,
    _: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    println!(
        "{}",
        args.into_iter()
            .map(|value| value.to_string())
            .collect::<Vec<String>>()
            .join("\t")
    );
    Ok(None)
}
pub fn _input(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        prefix => if ! Value::String(Default::default())
        => {
            let mut input = String::new();
            print!("{prefix}");
            let _ = std::io::stdout().flush();
            let _ = std::io::stdin().read_line(&mut input);
            input = input.trim_end().to_string();
            Ok(Some(Value::String(input)))
        }
    )
}
pub fn _setmeta(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::Object(object) => if ! Value::Object(Default::default()),
        meta => if ! Value::default()
        => {
            if let Value::Object(meta) = meta {
                object.borrow_mut().meta = Some(meta);
            }
            Ok(Some(Value::Object(object)))
        }
    )
}
pub fn _getmeta(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::Object(object) => if ! Value::Object(Default::default())
        => {
            let object = object.borrow();
            Ok(object.meta.clone().map(Value::Object))
        }
    )
}
pub fn _error(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(msg) => if ! Value::Int(Default::default())
        => {
            Err(PathLocated::new(Located::new(
                RunTimeError::Custom(msg),
                pos.clone(),
            ), path.to_string()))
        }
    )
}
pub fn _assert(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        cond => if ! Value::Bool(Default::default())
        => {
            if bool::from(&cond) {
                Ok(None)
            } else {
                Err(PathLocated::new(Located::new(
                    RunTimeError::Custom("assert failed!".to_string()),
                    pos.clone(),
                ), path.to_string()))
            }
        }
    )
}
pub fn _os_exit(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::Int(code) => if ! Value::Int(Default::default())
        => {
            std::process::exit(code as i32);
        }
    )
}
pub fn _ok(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::default()
        => {
            Ok(Some(Value::Result(Ok(Box::new(value)))))
        }
    )
}
pub fn _err(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::default()
        => {
            Ok(Some(Value::Result(Err(Box::new(value)))))
        }
    )
}

pub fn _type(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Null
        => {
            Ok(Some(Value::String(value.typ().to_string())))
        }
    )
}
pub fn _to_int(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Null
        => {
            match value {
                Value::Null => Ok(Some(Value::Int(0))),
                Value::Int(v) => Ok(Some(Value::Int(v))),
                Value::Float(v) => Ok(Some(Value::Int(v.abs() as isize))),
                Value::Bool(v) => Ok(Some(Value::Int(if v { 1 } else { 0 }))),
                Value::String(string) => Ok(Some(string.parse().ok().map(Value::Int).unwrap_or_default())),
                _ => Ok(None)
            }
        }
    )
}
pub fn _to_float(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Null
        => {
            match value {
                Value::Null => Ok(Some(Value::Float(0.))),
                Value::Int(v) => Ok(Some(Value::Float(v as f64))),
                Value::Float(v) => Ok(Some(Value::Float(v))),
                Value::Bool(v) => Ok(Some(Value::Float(if v { 1. } else { 0. }))),
                Value::String(string) => Ok(Some(string.parse().ok().map(Value::Float).unwrap_or_default())),
                _ => Ok(None)
            }
        }
    )
}
pub fn _to_bool(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
            value => if ! Value::Null
            => {
                Ok(Some(Value::Bool(bool::from(&value))))
            }
    )
}
pub fn _to_str(
    _: &mut Interpreter,
    args: Vec<Value>,
    _: &Positon,
    _: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    Ok(Some(Value::String(
        args.into_iter()
            .map(|arg| arg.to_string())
            .collect::<Vec<String>>()
            .join(""),
    )))
}
pub fn _to_vec(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    if args.len() > 1 {
        Ok(Some(Value::Vector(Rc::new(RefCell::new(args)))))
    } else {
        collect_args!(args pos path:
            value => if ! Value::Null
            => {
                match value {
                    Value::Null => Ok(Some(Value::Vector(Rc::new(RefCell::new(vec![]))))),
                    Value::String(string) => Ok(Some(Value::Vector(Rc::new(RefCell::new(string.chars().map(|c| Value::String(String::from(c))).collect()))))),
                    Value::Vector(vector) => Ok(Some(Value::Vector(vector))),
                    Value::Object(object) => {
                        let object = object.borrow();
                        Ok(Some(Value::Vector(Rc::new(RefCell::new(object.map.keys().map(|key| Value::String(key.clone())).collect())))))
                    }
                    _ => Ok(None)
                }
            }
        )
    }
}

pub fn _obj_copy(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::Object(object) => if ! Value::Object(Default::default())
        => {
            let object = object.borrow();
            Ok(Some(Value::Object(Rc::new(RefCell::new(object.clone())))))
        }
    )
}
pub fn _obj_deep_copy(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Object(Default::default())
        => {
            Ok(Some(value.deep_clone()))
        }
    )
}
pub fn _obj_keys(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::Object(object) => if ! Value::Object(Default::default())
        => {
            let object = object.borrow();
            Ok(Some(Value::Vector(Rc::new(RefCell::new(object.map.keys().map(|key| Value::String(key.clone())).collect())))))
        }
    )
}
pub fn _obj_values(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::Object(object) => if ! Value::Object(Default::default())
        => {
            let object = object.borrow();
            Ok(Some(Value::Vector(Rc::new(RefCell::new(object.map.values().cloned().collect())))))
        }
    )
}
pub fn _obj_get(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::Object(object) => if ! Value::Object(Default::default()),
        Value::String(key) => if ! Value::String(Default::default()),
        default => if ! Value::default()
        => {
            let object = object.borrow();
            Ok(Some(object.get(&key).unwrap_or(default)))
        }
    )
}

pub fn _str_get(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(string) => if ! Value::String(Default::default()),
        Value::Int(index) => if ! Value::Int(Default::default()),
        Value::String(default) => if ! Value::String(Default::default())
        => {
            let index = index.unsigned_abs();
            Ok(Some(Value::String(
                string
                    .get(index..=index)
                    .and_then(|s| s.chars().next().map(|c| c.to_string()))
                    .unwrap_or(default)
            )))
        }
    )
}
pub fn _str_len(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(string) => if ! Value::String(Default::default())
        => {
            Ok(Some(Value::Int(string.len() as isize)))
        }
    )
}
pub fn _str_char(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::Int(byte) => if ! Value::Int(Default::default())
        => {
            Ok(Some(Value::String((byte as u8 as char).to_string())))
        }
    )
}
pub fn _str_byte(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(string) => if ! Value::Int(Default::default()),
        index => if ! Value::Int(Default::default())
        => {
            let index = if let Value::Int(index) = index {
                index as usize
            } else {
                0
            };
            Ok(string.chars().nth(index).map(|c| c as u8 as isize).map(Value::Int))
        }
    )
}
pub fn _str_upper(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(string) => if ! Value::Int(Default::default())
        => {
            Ok(Some(Value::String(string.to_uppercase())))
        }
    )
}
pub fn _str_lower(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(string) => if ! Value::Int(Default::default())
        => {
            Ok(Some(Value::String(string.to_lowercase())))
        }
    )
}
pub fn _str_split(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(string) => if ! Value::String(Default::default()),
        Value::String(sep) => if ! Value::String(Default::default())
        => {
            let vector = string.split(sep.as_str()).map(|s| Value::String(s.to_string())).collect();
            Ok(Some(Value::Vector(Rc::new(RefCell::new(vector)))))
        }
    )
}
pub fn _str_is_digit(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(string) => if ! Value::Int(Default::default()),
        index => if ! Value::Int(Default::default())
        => {
            let index = if let Value::Int(index) = index {
                index as usize
            } else {
                0
            };
            Ok(string.chars().nth(index).map(|c| c.is_ascii_digit()).map(Value::Bool))
        }
    )
}
pub fn _str_is_radix(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(string) => if ! Value::Int(Default::default()),
        Value::Int(radix) => if ! Value::Int(Default::default()),
        index => if ! Value::Int(Default::default())
        => {
            let index = if let Value::Int(index) = index {
                index as usize
            } else {
                0
            };
            Ok(string.chars().nth(index).map(|c| c.is_digit(radix.unsigned_abs() as u32)).map(Value::Bool))
        }
    )
}
pub fn _str_is_alphabetic(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(string) => if ! Value::Int(Default::default()),
        index => if ! Value::Int(Default::default())
        => {
            let index = if let Value::Int(index) = index {
                index as usize
            } else {
                0
            };
            Ok(string.chars().nth(index).map(|c| c.is_ascii_alphabetic()).map(Value::Bool))
        }
    )
}
pub fn _str_is_alphanumeric(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(string) => if ! Value::Int(Default::default()),
        index => if ! Value::Int(Default::default())
        => {
            let index = if let Value::Int(index) = index {
                index as usize
            } else {
                0
            };
            Ok(string.chars().nth(index).map(|c| c.is_ascii_alphanumeric()).map(Value::Bool))
        }
    )
}
pub fn _str_is_whitespace(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(string) => if ! Value::Int(Default::default()),
        index => if ! Value::Int(Default::default())
        => {
            let index = if let Value::Int(index) = index {
                index as usize
            } else {
                0
            };
            Ok(string.chars().nth(index).map(|c| c.is_ascii_whitespace()).map(Value::Bool))
        }
    )
}
pub fn _str_is_lower(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(string) => if ! Value::Int(Default::default()),
        index => if ! Value::Int(Default::default())
        => {
            let index = if let Value::Int(index) = index {
                index as usize
            } else {
                0
            };
            Ok(string.chars().nth(index).map(|c| c.is_ascii_lowercase()).map(Value::Bool))
        }
    )
}
pub fn _str_is_upper(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(string) => if ! Value::Int(Default::default()),
        index => if ! Value::Int(Default::default())
        => {
            let index = if let Value::Int(index) = index {
                index as usize
            } else {
                0
            };
            Ok(string.chars().nth(index).map(|c| c.is_ascii_uppercase()).map(Value::Bool))
        }
    )
}
pub fn _str_is_punctuation(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(string) => if ! Value::Int(Default::default()),
        index => if ! Value::Int(Default::default())
        => {
            let index = if let Value::Int(index) = index {
                index as usize
            } else {
                0
            };
            Ok(string.chars().nth(index).map(|c| c.is_ascii_punctuation()).map(Value::Bool))
        }
    )
}
pub fn _str_format(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    let Value::String(string) = args.first().cloned().unwrap_or_default() else {
        return Err(PathLocated::new(
            Located::new(
                RunTimeError::Custom(format!(
                    "expected {} for argument #0, got {}",
                    Value::String(Default::default()),
                    args.first().cloned().unwrap_or_default()
                )),
                pos.clone(),
            ),
            path.to_string(),
        ));
    };
    let mut formatted_string = String::new();
    let values = args.into_iter().skip(1).collect::<Vec<Value>>();
    let mut idx = 0;
    let mut values_idx = 0;
    while let Some(c) = string.get(idx..=idx).and_then(|s| s.chars().next()) {
        if c == '$' {
            idx += 1;
            if let Some(c) = string.get(idx..=idx).and_then(|s| s.chars().next()) {
                match c {
                    's' => {
                        formatted_string += &values
                            .get(values_idx)
                            .cloned()
                            .unwrap_or_default()
                            .to_string();
                        values_idx += 1;
                    }
                    'q' => {
                        formatted_string +=
                            &match values.get(values_idx).cloned().unwrap_or_default() {
                                Value::String(string) => format!("{string:?}"),
                                value => value.to_string(),
                            };
                        values_idx += 1;
                    }
                    c if c.is_ascii_digit() => {
                        const ASCII_DIGIT_OFFSET: u8 = 48;
                        formatted_string += &values
                            .get((c as u8 - ASCII_DIGIT_OFFSET) as usize)
                            .cloned()
                            .unwrap_or_default()
                            .to_string();
                    }
                    c => {
                        formatted_string.push(c);
                    }
                }
            }
        } else {
            formatted_string.push(c);
        }
        idx += 1;
    }
    Ok(Some(Value::String(formatted_string)))
}

pub fn _vec_copy(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::Vector(vector) => if ! Value::Vector(Default::default())
        => {
            let vector = vector.borrow();
            Ok(Some(Value::Vector(Rc::new(RefCell::new(vector.clone())))))
        }
    )
}
pub fn _vec_get(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::Vector(vector) => if ! Value::Vector(Default::default()),
        Value::Int(index) => if ! Value::Int(Default::default()),
        default => if ! Value::default()
        => {
            let vector = vector.borrow();
            Ok(Some(vector.get(index.unsigned_abs()).cloned().unwrap_or(default)))
        }
    )
}
pub fn _vec_len(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::Vector(vector) => if ! Value::Vector(Default::default())
        => {
            let vector = vector.borrow();
            Ok(Some(Value::Int(vector.len() as isize)))
        }
    )
}
pub fn _vec_push(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
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
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::Vector(vector) => if ! Value::Vector(Default::default()),
        Value::Int(index) => if ! Value::Vector(Default::default()),
        value => if ! Value::default()
        => {
            let index = index.unsigned_abs();
            vector.borrow_mut().insert(index, value);
            Ok(None)
        }
    )
}
pub fn _vec_pop(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
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
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::Vector(vector) => if ! Value::Vector(Default::default()),
        Value::Int(index) => if ! Value::Vector(Default::default())
        => {
            let index = index.unsigned_abs();
            if vector.borrow().get(index).is_some() {
                return Ok(Some(vector.borrow_mut().remove(index)))
            }
            Ok(None)
        }
    )
}
pub fn _vec_pos(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::Vector(vector) => if ! Value::Vector(Default::default()),
        value => if ! Value::default()
        => {
            let vector = vector.borrow();
            Ok(vector.iter().position(|e| e == &value).map(|index| Value::Int(index as isize)))
        }
    )
}
pub fn _vec_range(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::Int(start) => if ! Value::Int(Default::default()),
        Value::Int(end) => if ! Value::Int(Default::default()),
        step => if ! Value::default()
        => {
            let step = if let Value::Int(step) = step {
                step
            } else {
                1
            };
            let mut vector = vec![];
            let mut i = start;
            if step < 0 {
                while i > end {
                    vector.push(Value::Int(i));
                    i += step;
                }
            } else {
                while i < end {
                    vector.push(Value::Int(i));
                    i += step;
                }
            }
            Ok(Some(Value::Vector(Rc::new(RefCell::new(vector)))))
        }
    )
}
pub fn _vec_join(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::Vector(vector) => if ! Value::Vector(Default::default()),
        Value::String(sep) => if ! Value::String(Default::default())
        => {
            let vector = vector.borrow();
            Ok(Some(Value::String(vector.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(sep.as_str()))))
        }
    )
}
pub fn _vec_sort(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::Vector(vector) => if ! Value::Vector(Default::default())
        => {
            let mut vector = vector.borrow().clone();
            vector.sort();
            Ok(Some(Value::Vector(Rc::new(RefCell::new(vector)))))
        }
    )
}
pub fn _vec_contains(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::Vector(vector) => if ! Value::Vector(Default::default()),
        value => if ! Value::default()
        => {
            let vector = vector.borrow().clone();
            Ok(Some(Value::Bool(vector.contains(&value))))
        }
    )
}

pub fn _math_floor(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Float(Default::default())
        => {
            match value {
                Value::Int(value) => Ok(Some(Value::Int(value))),
                Value::Float(value) => Ok(Some(Value::Float(value.floor()))),
                value => Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            }
        }
    )
}
pub fn _math_sqrt(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Float(Default::default())
        => {
            match value {
                Value::Int(value) => Ok(Some(Value::Float((value as f64).sqrt()))),
                Value::Float(value) => Ok(Some(Value::Float(value.sqrt()))),
                value => Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            }
        }
    )
}
pub fn _math_ceil(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Float(Default::default())
        => {
            match value {
                Value::Int(value) => Ok(Some(Value::Int(value))),
                Value::Float(value) => Ok(Some(Value::Float(value.ceil()))),
                value => Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            }
        }
    )
}
pub fn _math_round(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Float(Default::default())
        => {
            match value {
                Value::Int(value) => Ok(Some(Value::Int(value))),
                Value::Float(value) => Ok(Some(Value::Float(value.round()))),
                value => Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            }
        }
    )
}
pub fn _math_abs(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Float(Default::default())
        => {
            match value {
                Value::Int(value) => Ok(Some(Value::Int(value.abs()))),
                Value::Float(value) => Ok(Some(Value::Float(value.abs()))),
                value => Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            }
        }
    )
}
pub fn _math_cos(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f64,
                Value::Float(value) => value,
                value => return Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            };
            Ok(Some(Value::Float(value.cos())))
        }
    )
}
pub fn _math_sin(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f64,
                Value::Float(value) => value,
                value => return Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            };
            Ok(Some(Value::Float(value.sin())))
        }
    )
}
pub fn _math_tan(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f64,
                Value::Float(value) => value,
                value => return Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            };
            Ok(Some(Value::Float(value.tan())))
        }
    )
}
pub fn _math_acos(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f64,
                Value::Float(value) => value,
                value => return Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            };
            Ok(Some(Value::Float(value.acos())))
        }
    )
}
pub fn _math_asin(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f64,
                Value::Float(value) => value,
                value => return Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            };
            Ok(Some(Value::Float(value.asin())))
        }
    )
}
pub fn _math_atan(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f64,
                Value::Float(value) => value,
                value => return Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            };
            Ok(Some(Value::Float(value.atan())))
        }
    )
}
pub fn _math_cosh(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f64,
                Value::Float(value) => value,
                value => return Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            };
            Ok(Some(Value::Float(value.cosh())))
        }
    )
}
pub fn _math_sinh(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f64,
                Value::Float(value) => value,
                value => return Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            };
            Ok(Some(Value::Float(value.sinh())))
        }
    )
}
pub fn _math_tanh(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f64,
                Value::Float(value) => value,
                value => return Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            };
            Ok(Some(Value::Float(value.tanh())))
        }
    )
}
pub fn _math_acosh(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f64,
                Value::Float(value) => value,
                value => return Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            };
            Ok(Some(Value::Float(value.acosh())))
        }
    )
}
pub fn _math_asinh(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f64,
                Value::Float(value) => value,
                value => return Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            };
            Ok(Some(Value::Float(value.asinh())))
        }
    )
}
pub fn _math_atanh(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f64,
                Value::Float(value) => value,
                value => return Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            };
            Ok(Some(Value::Float(value.atanh())))
        }
    )
}
pub fn _math_atan2(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        value => if ! Value::Float(Default::default()),
        value2 => if ! Value::Float(Default::default())
        => {
            let value = match value {
                Value::Int(value) => value as f64,
                Value::Float(value) => value,
                value => return Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            };
            let value2 = match value2 {
                Value::Int(value) => value as f64,
                Value::Float(value) => value,
                value => return Err(PathLocated::new(Located::new(RunTimeError::Custom(format!("expected int/float for argument #0, got {}", value.typ())), pos.clone()), path.to_string()))
            };
            Ok(Some(Value::Float(value.atan2(value2))))
        }
    )
}

impl From<std::process::Output> for Value {
    fn from(val: std::process::Output) -> Self {
        Value::Result(if val.status.success() {
            Ok(Box::new(
                String::from_utf8(val.stderr)
                    .ok()
                    .map(Value::String)
                    .unwrap_or_default(),
            ))
        } else {
            Err(Box::new(
                String::from_utf8(val.stdout)
                    .ok()
                    .map(Value::String)
                    .unwrap_or_default(),
            ))
        })
    }
}
pub fn _os_execute(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(cmd) => if ! Value::String(Default::default())
        => {
            Ok(Some(std::process::Command::new(cmd)
                .args(args.into_iter().skip(1)
                    .map(|value| value.to_string())
                    .collect::<Vec<String>>())
                .output()
                .into()
            ))
        }
    )
}
pub fn _os_args(
    _: &mut Interpreter,
    _: Vec<Value>,
    _: &Positon,
    _: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    Ok(Some(Value::Vector(Rc::new(RefCell::new(
        std::env::args()
            .skip(1)
            .map(Value::String)
            .collect::<Vec<Value>>(),
    )))))
}
pub fn _os_var(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(var) => if ! Value::String(Default::default())
        => {
            Ok(std::env::var(var).map(Value::String).ok())
        }
    )
}
pub fn _os_set_var(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        var => if ! Value::String(Default::default()),
        value => if ! Value::String(Default::default())
        => {
            std::env::set_var(var.to_string(), value.to_string());
            Ok(None)
        }
    )
}
pub fn _os_env(
    _: &mut Interpreter,
    _: Vec<Value>,
    _: &Positon,
    _: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    Ok(Some(Value::Object(Rc::new(RefCell::new(Object {
        map: std::env::vars()
            .map(|(key, value)| (key, Value::String(value)))
            .collect(),
        meta: None,
    })))))
}

pub fn _fs_read(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(path) => if ! Value::String(Default::default())
        => {
            Ok(std::fs::read_to_string(path).ok().map(Value::String))
        }
    )
}
pub fn _fs_write(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(path) => if ! Value::String(Default::default()),
        Value::String(content) => if ! Value::String(Default::default())
        => {
            Ok(Some(std::fs::write(path, content).into()))
        }
    )
}
pub fn _fs_rename(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(old_path) => if ! Value::String(Default::default()),
        Value::String(new_path) => if ! Value::String(Default::default())
        => {
            Ok(Some(std::fs::rename(old_path, new_path).into()))
        }
    )
}
pub fn _fs_remove(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(path) => if ! Value::String(Default::default())
        => {
            Ok(Some(std::fs::remove_file(path).into()))
        }
    )
}
pub fn _fs_remove_dir(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(path) => if ! Value::String(Default::default())
        => {
            Ok(Some(std::fs::remove_dir(path).into()))
        }
    )
}

pub fn _require(
    interpreter: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(file_path) => if ! Value::String(Default::default())
        => {
            let current_file = interpreter.current_path().unwrap();
            let current_file_path = PathBuf::from_str(current_file).unwrap();
            let full_path = current_file_path
                .parent().unwrap()
                .to_str().unwrap()
                .to_string() + "/" + &file_path;
            let full_path = full_path.strip_prefix('/').unwrap_or(&full_path).to_string();
            let text = match fs::read_to_string(&full_path) {
                Ok(text) => text,
                Err(err) => return Err(PathLocated::new(Located::new(RunTimeError::FileNotFound(full_path.clone(), err.to_string()), pos.clone()), path.to_string()))
            };
            let closure = compile_ast(
                generate_ast(text)
                    .map_err(|err| err.map(CompileError::Parsing))
                    .map_err(|Located { value: err, pos }| PathLocated::new(Located::new(RunTimeError::Compiling(err), pos), full_path.clone()))?,
                &full_path
            ).map_err(|Located { value: err, pos }| PathLocated::new(Located::new(RunTimeError::Compiling(err), pos), full_path.clone()))?;
            let __module = interpreter.globals.remove("__module").unwrap_or_default();
            let value = interpreter.run(Rc::new(RefCell::new(closure)))
                .map_err(|traced| PathLocated::new(traced.err.located, full_path))?;
            interpreter.globals.insert("__module".to_string(), __module);
            Ok(value)
        }
    )
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::default()
    }
}
impl From<std::io::Error> for Value {
    fn from(val: std::io::Error) -> Self {
        Value::String(val.to_string())
    }
}
impl From<std::fmt::Error> for Value {
    fn from(val: std::fmt::Error) -> Self {
        Value::String(val.to_string())
    }
}
impl<T: Into<Value>, E: Into<Value>> From<Result<T, E>> for Value {
    fn from(val: Result<T, E>) -> Self {
        Value::Result(
            val.map(|v| Box::new(v.into()))
                .map_err(|err| Box::new(err.into())),
        )
    }
}
impl<T: Into<Value>> From<Option<T>> for Object {
    fn from(val: Option<T>) -> Self {
        let mut meta = HashMap::new();
        meta.insert("__name".to_string(), Value::String("option".to_string()));
        match val {
            Some(value) => {
                let mut map = HashMap::new();
                map.insert("type".to_string(), Value::String("some".to_string()));
                map.insert("value".to_string(), value.into());
                Object {
                    map,
                    meta: Some(Rc::new(RefCell::new(Object {
                        map: meta,
                        meta: None,
                    }))),
                }
            }
            None => {
                let mut map = HashMap::new();
                map.insert("type".to_string(), Value::String("none".to_string()));
                Object {
                    map,
                    meta: Some(Rc::new(RefCell::new(Object {
                        map: meta,
                        meta: None,
                    }))),
                }
            }
        }
    }
}
impl<T: TryFrom<Value, Error = ()>, E: TryFrom<Value, Error = ()>> TryFrom<Object>
    for Result<T, E>
{
    type Error = ();
    fn try_from(value: Object) -> Result<Self, Self::Error> {
        let Value::String(typ) = value.get("type").ok_or(())? else {
            return Err(());
        };
        match typ.as_str() {
            "ok" => Ok(Ok(T::try_from(value.get("value").unwrap_or_default())?)),
            "error" => Ok(Err(E::try_from(value.get("error").unwrap_or_default())?)),
            _ => Err(()),
        }
    }
}

pub struct SocketAddr {
    ip: String,
    port: u16
}
pub struct TcpStream {
    pub stream: std::net::TcpStream,
}
pub struct TcpListener {
    pub listener: std::net::TcpListener,
}
impl From<std::net::SocketAddr> for SocketAddr {
    fn from(value: std::net::SocketAddr) -> Self {
        Self { ip: value.ip().to_string(), port: value.port() }
    }
}
impl TcpStream {
    pub fn new(stream: std::net::TcpStream) -> Self {
        Self { stream }
    }
    pub fn read(
        _: &mut Interpreter,
        args: Vec<Value>,
        pos: &Positon,
        path: &str,
    ) -> Result<Option<Value>, PathLocated<RunTimeError>> {
        let arg = args.get(0).cloned().unwrap_or_default();
        let Value::ForeignObject(object) = arg else {
            return Err(PathLocated::new(
                Located::new(
                    RunTimeError::Custom(format!(
                        "expected {} for argument #0, got {}",
                        Value::Object(Default::default()).typ(),
                        Value::Null.typ()
                    )),
                    pos.clone(),
                ),
                path.to_string(),
            ));
        };
        let mut object = object.borrow_mut();
        object.call_mut("read", args, pos, path)
    }
    pub fn write(
        _: &mut Interpreter,
        args: Vec<Value>,
        pos: &Positon,
        path: &str,
    ) -> Result<Option<Value>, PathLocated<RunTimeError>> {
        let arg = args.get(0).cloned().unwrap_or_default();
        let Value::ForeignObject(object) = arg else {
            return Err(PathLocated::new(
                Located::new(
                    RunTimeError::Custom(format!(
                        "expected {} for argument #0, got {}",
                        Value::Object(Default::default()).typ(),
                        Value::Null.typ()
                    )),
                    pos.clone(),
                ),
                path.to_string(),
            ));
        };
        let mut object = object.borrow_mut();
        object.call_mut("write", args, pos, path)
    }
    pub fn shutdown(
        _: &mut Interpreter,
        args: Vec<Value>,
        pos: &Positon,
        path: &str,
    ) -> Result<Option<Value>, PathLocated<RunTimeError>> {
        let arg = args.get(0).cloned().unwrap_or_default();
        let Value::ForeignObject(object) = arg else {
            return Err(PathLocated::new(
                Located::new(
                    RunTimeError::Custom(format!(
                        "expected {} for argument #0, got {}",
                        Value::Object(Default::default()).typ(),
                        Value::Null.typ()
                    )),
                    pos.clone(),
                ),
                path.to_string(),
            ));
        };
        let object = object.borrow();
        object.call("shutdown", args, pos, path)
    }
}
impl TcpListener {
    pub fn new(listener: std::net::TcpListener) -> Self {
        Self { listener }
    }
    pub fn accept(
        _: &mut Interpreter,
        args: Vec<Value>,
        pos: &Positon,
        path: &str,
    ) -> Result<Option<Value>, PathLocated<RunTimeError>> {
        let arg = args.get(0).cloned().unwrap_or_default();
        let Value::ForeignObject(object) = arg else {
            return Err(PathLocated::new(
                Located::new(
                    RunTimeError::Custom(format!(
                        "expected {} for argument #0, got {}",
                        Value::Object(Default::default()).typ(),
                        Value::Null.typ()
                    )),
                    pos.clone(),
                ),
                path.to_string(),
            ));
        };
        let object = object.borrow();
        object.call("accept", args, pos, path)
    }
}
impl ForeignData for SocketAddr {
    fn get(&self, key: &str) -> Option<Value> {
        match key {
            "ip" => Some(Value::String(self.ip.clone())),
            "port" => Some(Value::Int(self.port as isize)),
            _ => None
        }
    }
}
impl ForeignData for TcpStream {
    fn get(&self, key: &str) -> Option<Value> {
        match key {
            "addr" => if let Ok(addr) = self.stream.peer_addr() {
                Some(Value::ForeignObject(Rc::new(RefCell::new(Box::new(SocketAddr::from(addr))))))
            } else {
                None
            },
            "read" => Some(Value::Function(FunctionKind::NativeFunction(Self::read))),
            "write" => Some(Value::Function(FunctionKind::NativeFunction(Self::write))),
            _ => None
        }
    }
    fn call_mut(&mut self, func: &str, args: Vec<Value>, _: &Positon, _: FilePathRef) -> Result<Option<Value>, PathLocated<RunTimeError>> {
        match func {
            "read" => {
                let mut buffer = String::new();
                if let Err(err) = self.stream.read_to_string(&mut buffer) {
                    return Ok(Some(Value::Result(Err(Box::new(Value::String(err.to_string()))))))
                }
                Ok(Some(Value::Result(Ok(Box::new(Value::String(buffer))))))
            }
            "write" => {
                let content = args.get(1).cloned().unwrap_or_default().to_string();
                if let Err(err) = self.stream.write(content.as_bytes()) {
                    return Ok(Some(Value::Result(Err(Box::new(Value::String(err.to_string()))))))
                }
                Ok(Some(Value::Result(Ok(Default::default()))))
            }
            "shutdown" => {
                if let Err(err) = self.stream.shutdown(std::net::Shutdown::Both) {
                    return Ok(Some(Value::Result(Err(Box::new(Value::String(err.to_string()))))))
                }
                Ok(Some(Value::Result(Ok(Default::default()))))
            }
            _ => Ok(None)
        }
    }
    fn call(&self, func: &str, _: Vec<Value>, _: &Positon, _: FilePathRef) -> Result<Option<Value>, PathLocated<RunTimeError>> {
        match func {
            "shutdown" => {
                if let Err(err) = self.stream.shutdown(std::net::Shutdown::Both) {
                    return Ok(Some(Value::Result(Err(Box::new(Value::String(err.to_string()))))))
                }
                Ok(Some(Value::Result(Ok(Default::default()))))
            }
            _ => Ok(None)
        }
    }
}
impl ForeignData for TcpListener {
    fn get(&self, key: &str) -> Option<Value> {
        match key {
            "accept" => Some(Value::Function(FunctionKind::NativeFunction(Self::accept))),
            _ => None,
        }
    }
    fn call(
        &self,
        func: &str,
        _: Vec<Value>,
        _: &Positon,
        _: FilePathRef,
    ) -> Result<Option<Value>, PathLocated<RunTimeError>> {
        match func {
            "accept" => {
                match self.listener.accept() {
                    Ok((stream, _)) => Ok(Some(Value::ForeignObject(Rc::new(RefCell::new(Box::new(TcpStream::new(stream))))))),
                    Err(err) => Ok(Some(Value::Result(Err(Box::new(Value::String(err.to_string())))))),
                }
            }
            _ => Ok(None),
        }
    }
}
impl Display for SocketAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.ip, self.port)
    }
}
impl Display for TcpStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.stream)
    }
}
impl Display for TcpListener {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.listener)
    }
}
pub fn _net_bind(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(addr) => if ! Value::String(Default::default()),
        Value::Int(ip) => if ! Value::Int(Default::default())
        => {
            let ip = ip.unsigned_abs() as u16;
            let Ok(listener) = std::net::TcpListener::bind((addr, ip)) else {
                return Ok(None)
            };
            Ok(Some(Value::ForeignObject(Rc::new(RefCell::new(Box::new(TcpListener::new(listener)))))))
        }
    )
}
pub fn _net_connect(
    _: &mut Interpreter,
    args: Vec<Value>,
    pos: &Positon,
    path: &str,
) -> Result<Option<Value>, PathLocated<RunTimeError>> {
    collect_args!(args pos path:
        Value::String(addr) => if ! Value::String(Default::default()),
        Value::Int(ip) => if ! Value::Int(Default::default())
        => {
            let ip = ip.unsigned_abs() as u16;
            let Ok(stream) = std::net::TcpStream::connect((addr, ip)) else {
                return Ok(None)
            };
            Ok(Some(Value::ForeignObject(Rc::new(RefCell::new(Box::new(TcpStream::new(stream)))))))
        }
    )
}
