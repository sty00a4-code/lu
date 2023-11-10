use oneparse::position::{Located, Positon};
use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    error::Error,
    fmt::{Debug, Display},
    rc::Rc, cmp::Ordering,
};

use crate::{
    compiler::{ByteCode, Closure, Location, Source},
    parser::{BinaryOperator, CompileError, UnaryOperator},
};

#[derive(Clone, Default)]
pub enum Value {
    #[default]
    Null,
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Vector(Rc<RefCell<Vec<Self>>>),
    Object(Rc<RefCell<Object>>),
    Function(FunctionKind),
}
#[derive(Clone)]
pub enum FunctionKind {
    Function(Rc<RefCell<Closure>>),
    NativeFunction(NativeFunction),
}
pub type FilePath = String;
pub type FilePathRef<'a> = &'a str;
pub type NativeFunction =
    fn(&mut Interpreter, Vec<Value>, &Positon, FilePathRef) -> Result<Option<Value>, PathLocated<RunTimeError>>;
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Object {
    pub map: HashMap<String, Value>,
    pub meta: Option<Rc<RefCell<Self>>>,
}
impl Object {
    pub fn get(&self, key: &str) -> Option<Value> {
        self.map.get(key).cloned()
    }
    pub fn get_meta(&self, key: &str) -> Option<Value> {
        if let Some(meta) = &self.meta {
            Rc::clone(meta).borrow().get(key)
        } else {
            None
        }
    }
}

#[derive(Debug, Default)]
pub struct Interpreter {
    pub globals: HashMap<String, Value>,
    pub call_stack: Vec<CallFrame>,
    pub stack: Vec<Value>,
}
#[derive(Debug, Clone)]
pub struct CallFrame {
    pub closure: RefCell<Closure>,
    pub ip: usize,
    pub stack_base: usize,
    pub dst: Option<Location>,
    pub path: String,
}

pub const GLOBAL_NULL: &Value = &Value::Null;
#[derive(Debug, Clone, PartialEq)]
pub enum RunTimeError {
    Binary(BinaryOperator, Value, Value),
    Unary(UnaryOperator, Value),
    NotCallable(Value),
    InvalidFieldHead(Value),
    InvalidField(Value, Value),
    Custom(String),
    Compiling(CompileError),
    FileNotFound(String, String),
}
#[derive(Debug, Clone)]
pub struct Traced<E> {
    pub trace: Vec<(String, Positon)>,
    pub err: E,
}
pub struct PathLocated<T> {
    pub located: Located<T>,
    pub path: FilePath
}


impl<T> PathLocated<T> {
    pub fn new(located: Located<T>, path: FilePath) -> Self {
        Self { located, path }
    }
}
impl Interpreter {
    pub fn with_globals(mut self, globals: HashMap<String, Value>) -> Self {
        self.globals = globals;
        self
    }
    pub fn enter_call(
        &mut self,
        closure: Rc<RefCell<Closure>>,
        args: Vec<Value>,
        dst: Option<Location>,
    ) {
        let call_frame = CallFrame {
            closure: RefCell::clone(&closure),
            ip: 0,
            stack_base: self.stack.len(),
            dst,
            path: closure.borrow().path.clone(),
        };
        for i in 0..call_frame.closure.borrow().args {
            self.stack.push(args.get(i).cloned().unwrap_or_default());
        }
        for _ in call_frame.closure.borrow().args..call_frame.closure.borrow().registers {
            self.stack.push(Value::default());
        }
        self.call_stack.push(call_frame);
    }
    pub fn return_call(&mut self, value: Value) -> Option<CallFrame> {
        let call_frame = self.call_stack.pop();
        if let Some(call_frame) = &call_frame {
            while self.stack.len() > call_frame.stack_base {
                self.stack.pop();
            }
            if let Some(dst) = call_frame.dst {
                if self.call_stack.is_empty() {
                    self.call_stack.push(call_frame.clone());
                    let register = self.location(dst).expect("location not found");
                    *register = value;
                    self.call_stack.pop();
                } else {
                    let register = self.location(dst).expect("location not found");
                    *register = value;
                }
            }
        }
        call_frame
    }
    pub fn current_call_frame(&self) -> Option<&CallFrame> {
        self.call_stack.last()
    }
    pub fn current_call_frame_mut(&mut self) -> Option<&mut CallFrame> {
        self.call_stack.last_mut()
    }
    pub fn current_path(&self) -> Option<&String> {
        Some(&self.current_call_frame()?.path)
    }
    pub fn current_closure(&self) -> Option<Ref<Closure>> {
        self.current_call_frame()
            .map(|call_frame| call_frame.closure.borrow())
    }
    pub fn current_instr(&self) -> Option<Located<ByteCode>> {
        let closure = self.current_closure()?;
        let ip = self
            .current_call_frame()?
            .ip;
        closure.code.get(ip).cloned()
    }
    pub fn register(&self, register: usize) -> Option<&Value> {
        let stack_base = self
            .current_call_frame()?
            .stack_base;
        self.stack.get(stack_base + register)
    }
    pub fn register_mut(&mut self, register: usize) -> Option<&mut Value> {
        let stack_base = self
            .current_call_frame()?
            .stack_base;
        self.stack.get_mut(stack_base + register)
    }
    pub fn constant(&self, addr: usize) -> Option<Value> {
        let consts = &self.current_closure().expect("no current closure").consts;
        consts.get(addr).cloned()
    }
    pub fn source(&self, source: Source) -> Option<Value> {
        match source {
            Source::Register(register) => self.register(register).cloned(),
            Source::Const(addr) => self.constant(addr),
            Source::Null => Some(Value::Null),
            Source::Int(v) => Some(Value::Int(v)),
            Source::Float(v) => Some(Value::Float(v)),
            Source::Bool(v) => Some(Value::Bool(v)),
            Source::Global(addr) => {
                let ident = match self.constant(addr)? {
                    Value::String(ident) => ident.clone(),
                    _ => return None,
                };
                self.globals.get(ident.as_str()).cloned()
            }
        }
    }
    pub fn location(&mut self, location: Location) -> Option<&mut Value> {
        match location {
            Location::Register(register) => self.register_mut(register),
            Location::Global(addr) => {
                let ident = match self.constant(addr)? {
                    Value::String(ident) => ident.clone(),
                    _ => return None,
                };
                if !self.globals.contains_key(ident.as_str()) {
                    self.globals.insert(ident.to_string(), Value::default());
                }
                self.globals.get_mut(ident.as_str())
            }
        }
    }

    pub fn create_trace(&self, err: PathLocated<RunTimeError>, offset: usize) -> Traced<PathLocated<RunTimeError>> {
        let PathLocated { located: Located { value: err, pos }, path } = err;
        let mut trace = vec![];
        trace.push((path.clone(), pos.clone()));
        for i in 0..(self.call_stack.len() - offset) {
            let call_frame = self.call_stack.get(self.call_stack.len() - i - 1).unwrap();
            trace.push((
                call_frame.path.clone(),
                call_frame
                    .closure
                    .borrow()
                    .code
                    .get(call_frame.ip)
                    .map(|loc| loc.pos.clone())
                    .unwrap_or_default(),
            ));
        }
        Traced { trace, err: PathLocated::new(Located::new(err, pos), path.clone()) }
    }
    pub fn run(
        &mut self,
        closure: Rc<RefCell<Closure>>,
    ) -> Result<Option<Value>, Traced<PathLocated<RunTimeError>>> {
        let call_offset = self.call_stack.len();
        self.enter_call(closure, vec![], Some(Location::Global(0)));
        loop {
            if self.step().map_err(|err| self.create_trace(err, call_offset))? {
                while self.call_stack.len() > call_offset {
                    self.call_stack.pop();
                }
                break;
            }
            if self.call_stack.len() == call_offset {
                break;
            }
        }
        Ok(self.globals.get("__module").cloned().and_then(|value| {
            if value == Value::Null {
                None
            } else {
                Some(value)
            }
        }))
    }
    pub fn step(&mut self) -> Result<bool, PathLocated<RunTimeError>> {
        let Located {
            value: bytecode,
            pos,
        } = self
            .current_instr()
            .expect("no current instruction")
            .clone();
        {
            let ip = &mut self
                .current_call_frame_mut()
                .expect("no current call frame")
                .ip;
            *ip += 1;
        }
        match bytecode {
            ByteCode::None => {}
            ByteCode::Halt => return Ok(true),
            ByteCode::Jump { addr } => {
                let ip = &mut self
                    .current_call_frame_mut()
                    .expect("no current call frame")
                    .ip;
                *ip = addr;
            }
            ByteCode::JumpIf { cond, addr, not } => {
                let cond = self.source(cond).expect("source not found");
                let cond = if not {
                    !bool::from(&cond)
                } else {
                    bool::from(&cond)
                };
                if cond {
                    let ip = &mut self
                        .current_call_frame_mut()
                        .expect("no current call frame")
                        .ip;
                    *ip = addr;
                }
            }
            ByteCode::Call {
                func,
                start,
                amount,
                dst,
            } => {
                let func = self.source(func).unwrap_or_default();
                let mut args = vec![];
                for addr in start..start + amount {
                    args.push(
                        self.source(Source::Register(addr))
                            .expect("source not found")
                            .clone(),
                    );
                }
                match func {
                    Value::Function(kind) => match kind {
                        FunctionKind::Function(closure) => {
                            self.enter_call(closure, args, dst);
                        }
                        FunctionKind::NativeFunction(func) => {
                            let path = self.current_call_frame().expect("no call frame on call stack").path.clone();
                            let value = func(self, args, &pos, &path)?;
                            if let Some(dst) = dst {
                                let register = self.location(dst).expect("location not found");
                                *register = value.unwrap_or_default();
                            }
                        }
                    },
                    value => return Err(PathLocated::new(Located::new(RunTimeError::NotCallable(value), pos), self.current_path().expect("no current path found").clone())),
                }
            }
            ByteCode::SelfCall {
                head,
                field,
                start,
                amount,
                dst,
            } => {
                let head = self.source(head).unwrap_or_default();
                let field = self.source(field).unwrap_or_default();
                let func = match &head {
                    Value::Object(object) => match field {
                        Value::String(field) => object
                            .borrow()
                            .map
                            .get(field.as_str())
                            .cloned()
                            .unwrap_or_default(),
                        field => {
                            return Err(PathLocated::new(Located::new(RunTimeError::InvalidField(head, field), pos), self.current_path().expect("no current path found").clone()))
                        }
                    },
                    Value::Vector(_) => match field {
                        Value::String(field) => {
                            if let Some(Value::Object(vec_module)) = self.globals.get("vec") {
                                vec_module.borrow().get(&field).unwrap_or_default()
                            } else {
                                return Err(PathLocated::new(Located::new(
                                    RunTimeError::InvalidField(head, Value::String(field)),
                                    pos,
                                ), self.current_path().expect("no current path found").clone()));
                            }
                        }
                        field => {
                            return Err(PathLocated::new(Located::new(RunTimeError::InvalidField(head, field), pos), self.current_path().expect("no current path found").clone()))
                        }
                    },
                    Value::String(_) => match field {
                        Value::String(field) => {
                            if let Some(Value::Object(str_module)) = self.globals.get("str") {
                                str_module.borrow().get(&field).unwrap_or_default()
                            } else {
                                return Err(PathLocated::new(Located::new(
                                    RunTimeError::InvalidField(head, Value::String(field)),
                                    pos,
                                ), self.current_path().expect("no current path found").clone()));
                            }
                        }
                        field => {
                            return Err(PathLocated::new(Located::new(RunTimeError::InvalidField(head, field), pos), self.current_path().expect("no current path found").clone()))
                        }
                    },
                    _ => return Err(PathLocated::new(Located::new(RunTimeError::InvalidFieldHead(head), pos), self.current_path().expect("no current path found").clone())),
                };
                let mut args = vec![head.clone()];
                for addr in start..start + amount {
                    args.push(
                        self.source(Source::Register(addr))
                            .expect("source not found")
                            .clone(),
                    );
                }
                match func {
                    Value::Function(kind) => match kind {
                        FunctionKind::Function(closure) => {
                            self.enter_call(closure, args, dst);
                        }
                        FunctionKind::NativeFunction(func) => {
                            let path = self.current_call_frame().expect("no call frame on call stack").path.clone();
                            let value = func(self, args, &pos, &path)?;
                            if let Some(dst) = dst {
                                let register = self.location(dst).expect("location not found");
                                *register = value.unwrap_or_default();
                            }
                        }
                    },
                    value => return Err(PathLocated::new(Located::new(RunTimeError::NotCallable(value), pos), self.current_path().expect("no current path found").clone())),
                }
            }
            ByteCode::Return { src } => {
                let return_value = self.source(src).unwrap_or_default();
                self.return_call(return_value);
                return Ok(self.call_stack.is_empty())
            }

            ByteCode::Move { dst, src } => {
                let value = self.source(src).unwrap_or_default();
                let register = self.location(dst).expect("location not found");
                *register = value;
            }
            ByteCode::Null { dst } => {
                let register = self.location(dst).expect("location not found");
                *register = Value::Null;
            }
            ByteCode::Vector { dst, start, amount } => {
                let mut vector = vec![];
                for addr in start..start + amount {
                    vector.push(
                        self.source(Source::Register(addr))
                            .expect("source not found")
                            .clone(),
                    );
                }
                let register = self.location(dst).expect("location not found");
                *register = Value::Vector(Rc::new(RefCell::new(vector)));
            }
            ByteCode::Object { dst } => {
                let register = self.location(dst).expect("location not found");
                *register = Value::Object(Rc::new(RefCell::new(Object::default())));
            }
            ByteCode::SetField { dst, field, src } => {
                let field = self.source(field).unwrap_or_default();
                let value = self.source(src).unwrap_or_default();
                match self.location(dst).expect("location not found") {
                    Value::Object(object) => {
                        if let Value::String(field) = field {
                            object.borrow_mut().map.insert(field.clone(), value);
                        } else {
                            return Err(PathLocated::new(Located::new(
                                RunTimeError::InvalidField(Value::Object(object.clone()), field),
                                pos,
                            ), self.current_path().expect("no current path found").clone()));
                        }
                    }
                    Value::Vector(vector) => {
                        if let Value::Int(index) = field {
                            if let Some(old_value) =
                                vector.borrow_mut().get_mut(index.unsigned_abs() as usize)
                            {
                                *old_value = value;
                            }
                        } else {
                            return Err(PathLocated::new(Located::new(
                                RunTimeError::InvalidField(Value::Vector(vector.clone()), field),
                                pos,
                            ), self.current_path().expect("no current path found").clone()));
                        }
                    }
                    value => {
                        return Err(PathLocated::new(Located::new(
                            RunTimeError::InvalidFieldHead(value.clone()),
                            pos,
                        ), self.current_path().expect("no current path found").clone()))
                    }
                }
            }

            ByteCode::Binary {
                op,
                dst,
                left,
                right,
            } => {
                let left = self.source(left).unwrap_or_default();
                let right = self.source(right).unwrap_or_default();
                let register = self.location(dst).expect("location not found");
                *register = match op {
                    BinaryOperator::And => {
                        if bool::from(&left) {
                            right
                        } else {
                            left
                        }
                    }
                    BinaryOperator::Or => {
                        if bool::from(&left) {
                            left
                        } else {
                            right
                        }
                    }
                    BinaryOperator::EQ => Value::Bool(left == right),
                    BinaryOperator::NE => Value::Bool(left != right),
                    BinaryOperator::LT => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Bool(left < right),
                        (Value::Float(left), Value::Float(right)) => Value::Bool(left < right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Bool((left as f32) < right)
                        }
                        (Value::Float(left), Value::Int(right)) => Value::Bool(left < right as f32),
                        (left, right) => {
                            return Err(PathLocated::new(Located::new(
                                RunTimeError::Binary(op, left, right),
                                pos.clone(),
                            ), self.current_path().expect("no current path found").clone()))
                        }
                    },
                    BinaryOperator::GT => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Bool(left > right),
                        (Value::Float(left), Value::Float(right)) => Value::Bool(left > right),
                        (Value::Int(left), Value::Float(right)) => Value::Bool(left as f32 > right),
                        (Value::Float(left), Value::Int(right)) => Value::Bool(left > right as f32),
                        (left, right) => {
                            return Err(PathLocated::new(Located::new(
                                RunTimeError::Binary(op, left, right),
                                pos.clone(),
                            ), self.current_path().expect("no current path found").clone()))
                        }
                    },
                    BinaryOperator::LE => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Bool(left <= right),
                        (Value::Float(left), Value::Float(right)) => Value::Bool(left <= right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Bool(left as f32 <= right)
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Bool(left <= right as f32)
                        }
                        (left, right) => {
                            return Err(PathLocated::new(Located::new(
                                RunTimeError::Binary(op, left, right),
                                pos.clone(),
                            ), self.current_path().expect("no current path found").clone()))
                        }
                    },
                    BinaryOperator::GE => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Bool(left >= right),
                        (Value::Float(left), Value::Float(right)) => Value::Bool(left >= right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Bool(left as f32 >= right)
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Bool(left >= right as f32)
                        }
                        (left, right) => {
                            return Err(PathLocated::new(Located::new(
                                RunTimeError::Binary(op, left, right),
                                pos.clone(),
                            ), self.current_path().expect("no current path found").clone()))
                        }
                    },
                    BinaryOperator::Add => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Int(left + right),
                        (Value::Float(left), Value::Float(right)) => Value::Float(left + right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Float(left as f32 + right)
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Float(left + right as f32)
                        }
                        (Value::String(left), Value::String(right)) => Value::String(left + &right),
                        (left, right) => {
                            return Err(PathLocated::new(Located::new(
                                RunTimeError::Binary(op, left, right),
                                pos.clone(),
                            ), self.current_path().expect("no current path found").clone()))
                        }
                    },
                    BinaryOperator::Sub => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Int(left - right),
                        (Value::Float(left), Value::Float(right)) => Value::Float(left - right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Float(left as f32 - right)
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Float(left - right as f32)
                        }
                        (left, right) => {
                            return Err(PathLocated::new(Located::new(
                                RunTimeError::Binary(op, left, right),
                                pos.clone(),
                            ), self.current_path().expect("no current path found").clone()))
                        }
                    },
                    BinaryOperator::Div => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            Value::Float(left as f32 / right as f32)
                        }
                        (Value::Float(left), Value::Float(right)) => Value::Float(left / right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Float(left as f32 / right)
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Float(left / right as f32)
                        }
                        (left, right) => {
                            return Err(PathLocated::new(Located::new(
                                RunTimeError::Binary(op, left, right),
                                pos.clone(),
                            ), self.current_path().expect("no current path found").clone()))
                        }
                    },
                    BinaryOperator::Mul => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Int(left * right),
                        (Value::Float(left), Value::Float(right)) => Value::Float(left * right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Float(left as f32 * right)
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Float(left * right as f32)
                        }
                        (Value::String(left), Value::Int(right)) => {
                            Value::String(left.repeat(right.unsigned_abs() as usize))
                        }
                        (left, right) => {
                            return Err(PathLocated::new(Located::new(
                                RunTimeError::Binary(op, left, right),
                                pos.clone(),
                            ), self.current_path().expect("no current path found").clone()))
                        }
                    },
                    BinaryOperator::Mod => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Int(left % right),
                        (Value::Float(left), Value::Float(right)) => Value::Float(left % right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Float(left as f32 % right)
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Float(left % right as f32)
                        }
                        (left, right) => {
                            return Err(PathLocated::new(Located::new(
                                RunTimeError::Binary(op, left, right),
                                pos.clone(),
                            ), self.current_path().expect("no current path found").clone()))
                        }
                    },
                    BinaryOperator::Pow => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            Value::Float((left as f32).powf(right as f32))
                        }
                        (Value::Float(left), Value::Float(right)) => Value::Float(left.powf(right)),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Float((left as f32).powf(right))
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Float(left.powf(right as f32))
                        }
                        (left, right) => {
                            return Err(PathLocated::new(Located::new(
                                RunTimeError::Binary(op, left, right),
                                pos.clone(),
                            ), self.current_path().expect("no current path found").clone()))
                        }
                    },
                };
            }
            ByteCode::Unary { op, dst, right } => {
                let right = self.source(right).unwrap_or_default();
                let register = self.location(dst).expect("location not found");
                *register = match op {
                    UnaryOperator::Neg => match right {
                        Value::Int(right) => Value::Int(-right),
                        Value::Float(right) => Value::Float(-right),
                        right => {
                            return Err(PathLocated::new(Located::new(RunTimeError::Unary(op, right), pos.clone()), self.current_path().expect("no current path found").clone()))
                        }
                    },
                    UnaryOperator::Not => Value::Bool(!bool::from(&right)),
                };
            }
            ByteCode::Field { dst, head, field } => {
                let head = self.source(head).unwrap_or_default();
                let field = self.source(field).unwrap_or_default();
                let register = self.location(dst).expect("location not found");
                *register = match &head {
                    Value::Object(object) => match field {
                        Value::String(field) => object
                            .borrow()
                            .map
                            .get(field.as_str())
                            .cloned()
                            .unwrap_or_default(),
                        field => {
                            return Err(PathLocated::new(Located::new(RunTimeError::InvalidField(head, field), pos), self.current_path().expect("no current path found").clone()))
                        }
                    },
                    Value::Vector(vector) => match field {
                        Value::Int(index) => vector
                            .borrow()
                            .get(index as u32 as usize)
                            .cloned()
                            .unwrap_or_default(),
                        field => {
                            return Err(PathLocated::new(Located::new(RunTimeError::InvalidField(head, field), pos), self.current_path().expect("no current path found").clone()))
                        }
                    },
                    Value::String(string) => match field {
                        Value::Int(index) => string
                            .chars()
                            .collect::<Vec<char>>()
                            .get(index.unsigned_abs() as usize)
                            .map(|c| Value::String(String::from(*c)))
                            .unwrap_or_default(),
                        field => {
                            return Err(PathLocated::new(Located::new(RunTimeError::InvalidField(head, field), pos), self.current_path().expect("no current path found").clone()))
                        }
                    },
                    _ => return Err(PathLocated::new(Located::new(RunTimeError::InvalidFieldHead(head), pos), self.current_path().expect("no current path found").clone())),
                };
            }
        }
        Ok(false)
    }
}

impl Value {
    pub fn typ(&self) -> &'static str {
        match self {
            Value::Null => "null",
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::Bool(_) => "bool",
            Value::String(_) => "string",
            Value::Vector(_) => "vector",
            Value::Object(_) => "object",
            Value::Function(_) => "function",
        }
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Null, Self::Null) => true,
            (Self::Int(left), Self::Int(right)) => left == right,
            (Self::Float(left), Self::Float(right)) => left == right,
            (Self::Int(left), Self::Float(right)) => *left as f32 == *right,
            (Self::Float(left), Self::Int(right)) => *left == *right as f32,
            (Self::Bool(left), Self::Bool(right)) => left == right,
            (Self::String(left), Self::String(right)) => left == right,
            (Self::Vector(left), Self::Vector(right)) => std::ptr::eq(left, right),
            (Self::Object(left), Self::Object(right)) => std::ptr::eq(left, right),
            (Self::Function(left), Self::Function(right)) => std::ptr::eq(left, right),
            _ => false,
        }
    }
}
impl Eq for Value {

}
impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Int(v) => write!(f, "{v}"),
            Value::Float(v) => write!(f, "{v}"),
            Value::Bool(v) => write!(f, "{v}"),
            Value::String(string) => write!(f, "{string}"),
            Value::Vector(vector) => write!(f, "{:?}", vector.borrow()),
            Value::Object(object) => write!(
                f,
                "{}:{:?}",
                if let Some(name) = object.borrow().get_meta("__name") {
                    name.to_string()
                } else {
                    "object".to_string()
                },
                object.as_ptr()
            ),
            Value::Function(kind) => write!(f, "function:{kind}"),
        }
    }
}
impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Int(v) => write!(f, "{v:?}"),
            Value::Float(v) => write!(f, "{v:?}"),
            Value::Bool(v) => write!(f, "{v:?}"),
            Value::String(string) => write!(f, "{string:?}"),
            Value::Vector(vector) => write!(f, "{:?}", vector.borrow()),
            Value::Object(object) => write!(f, "object:{:?}", object.as_ptr()),
            Value::Function(kind) => write!(f, "function:{kind}"),
        }
    }
}
impl Display for FunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionKind::Function(closure) => {
                write!(f, "{:?}", closure.as_ptr() as *const Closure)
            }
            FunctionKind::NativeFunction(func) => write!(f, "{:?}", func as *const NativeFunction),
        }
    }
}
impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) }
}
impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Value::Int(left), Value::Int(right)) => left.cmp(right),
            (Value::Float(left), Value::Float(right)) => left.total_cmp(right),
            (Value::Int(left), Value::Float(right)) => (*left as f32).total_cmp(right),
            (Value::Float(left), Value::Int(right)) => left.total_cmp(&(*right as f32)),
            _ => Ordering::Equal
        }
    }
}

impl From<&Value> for bool {
    fn from(value: &Value) -> bool {
        match value {
            Value::Null => false,
            Value::Int(v) => *v != 0,
            Value::Float(v) => *v != 0.,
            Value::Bool(v) => *v,
            Value::String(string) => !string.is_empty(),
            Value::Vector(vector) => !vector.borrow().is_empty(),
            Value::Object(_) => true,
            Value::Function(_) => true,
        }
    }
}
impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Self::Int(value)
    }
}
impl From<f32> for Value {
    fn from(value: f32) -> Self {
        Self::Float(value)
    }
}
impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}
impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}
impl From<HashMap<String, Value>> for Value {
    fn from(value: HashMap<String, Value>) -> Self {
        Self::Object(Rc::new(RefCell::new(Object::from(value))))
    }
}
impl From<Closure> for Value {
    fn from(value: Closure) -> Self {
        Self::Function(FunctionKind::Function(Rc::new(RefCell::new(value))))
    }
}
impl From<NativeFunction> for Value {
    fn from(value: NativeFunction) -> Self {
        Self::Function(FunctionKind::NativeFunction(value))
    }
}
impl From<HashMap<String, Value>> for Object {
    fn from(value: HashMap<String, Value>) -> Self {
        Self {
            map: value,
            meta: None,
        }
    }
}
impl<T: Into<Value>> From<Option<T>> for Value {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(value) => value.into(),
            None => Value::Null,
        }
    }
}
impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(value: Vec<T>) -> Self {
        Self::Vector(Rc::new(RefCell::new(
            value.into_iter().map(|value| value.into()).collect(),
        )))
    }
}
impl TryFrom<Value> for i32 {
    type Error = ();
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Int(value) => Ok(value),
            _ => Err(()),
        }
    }
}
impl TryFrom<Value> for f32 {
    type Error = ();
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Float(value) => Ok(value),
            _ => Err(()),
        }
    }
}
impl TryFrom<Value> for bool {
    type Error = ();
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Bool(value) => Ok(value),
            _ => Err(()),
        }
    }
}
pub enum TryFromValueRcError<T> {
    MissmatchedValue,
    CantGetInnerValue(RefCell<T>),
}
impl<T> Display for TryFromValueRcError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TryFromValueRcError::MissmatchedValue => write!(f, "missmatched value types"),
            TryFromValueRcError::CantGetInnerValue(_) => write!(f, "can't get the inner value"),
        }
    }
}
impl TryFrom<Value> for String {
    type Error = TryFromValueRcError<Self>;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(value) => Ok(value),
            _ => Err(TryFromValueRcError::MissmatchedValue),
        }
    }
}
impl TryFrom<Value> for Vec<Value> {
    type Error = TryFromValueRcError<Self>;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Vector(value) => Ok(value.borrow().clone()),
            _ => Err(TryFromValueRcError::MissmatchedValue),
        }
    }
}
impl TryFrom<Value> for Object {
    type Error = TryFromValueRcError<Self>;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Object(value) => Ok(value.borrow().clone()),
            _ => Err(TryFromValueRcError::MissmatchedValue),
        }
    }
}

impl Display for RunTimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunTimeError::Binary(op, left, right) => {
                write!(
                    f,
                    "cannot perform '{op}' on {} with {}",
                    left.typ(),
                    right.typ()
                )
            }
            RunTimeError::Unary(op, right) => write!(f, "cannot perform '{op}' on {}", right.typ()),
            RunTimeError::NotCallable(value) => write!(f, "cannot call {}", value.typ()),
            RunTimeError::InvalidFieldHead(head) => write!(f, "cannot get field of {}", head.typ()),
            RunTimeError::InvalidField(head, field) => {
                write!(f, "cannot get field of {} with {}", head.typ(), field.typ())
            }
            RunTimeError::Custom(string) => write!(f, "{string}"),
            RunTimeError::Compiling(err) => write!(f, "{err}"),
            RunTimeError::FileNotFound(path, err) => write!(f, "error while trying to read {path:?}: {err}"),
        }
    }
}
impl Error for RunTimeError {}
