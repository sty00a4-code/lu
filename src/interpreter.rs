use oneparse::position::Located;
use std::{
    collections::HashMap,
    error::Error,
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{
    compiler::{ByteCode, Closure, Location, Source},
    parser::{BinaryOperator, UnaryOperator},
};

#[derive(Clone, Default)]
pub enum Value {
    #[default]
    Null,
    Int(i32),
    Float(f32),
    Bool(bool),
    String(Rc<String>),
    Vector(Rc<Vec<Self>>),
    Object(Rc<Object>),
    Function(FunctionKind),
}
#[derive(Clone)]
pub enum FunctionKind {
    Function(Rc<Closure>),
    NativeFunction(NativeFunction)
}
pub type NativeFunction = fn(&mut Interpreter, Vec<Value>) -> Result<Option<Value>, Located<RunTimeError>>;
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Object {
    map: HashMap<String, Value>,
    meta: Option<Rc<Self>>,
}

#[derive(Debug, Default)]
pub struct Interpreter {
    pub globals: HashMap<String, Value>,
    pub call_stack: Vec<CallFrame>,
    pub stack: Vec<Value>,
}
#[derive(Debug, Clone)]
pub struct CallFrame {
    pub closure: Rc<Closure>,
    pub ip: usize,
    pub stack_base: usize,
    pub dst: Option<Location>,
}

pub const GLOBAL_NULL: &Value = &Value::Null;
#[derive(Debug, Clone, PartialEq)]
pub enum RunTimeError {
    Binary(BinaryOperator, Value, Value),
    Unary(UnaryOperator, Value),
    NotCallable(Value),
    InvalidFieldHead(Value),
    InvalidField(Value, Value),
}
impl Interpreter {
    pub fn with_globals(mut self, globals: HashMap<String, Value>) -> Self {
        self.globals = globals;
        self
    }
    pub fn enter_call(&mut self, closure: Rc<Closure>, args: Vec<Value>, dst: Option<Location>) {
        let call_frame = CallFrame {
            closure: Rc::clone(&closure),
            ip: 0,
            stack_base: self.stack.len(),
            dst,
        };
        for i in 0..call_frame.closure.registers {
            self.stack.push(args.get(i).cloned().unwrap_or_default());
        }
        self.call_stack.push(call_frame);
    }
    pub fn return_call(&mut self, value: Value) -> Option<CallFrame> {
        let call_frame = self.call_stack.pop();
        if let Some(call_frame) = &call_frame {
            for _ in 0..call_frame.closure.registers {
                self.stack.pop();
            }
            while self.stack.len() > call_frame.stack_base {
                self.stack.pop();
            }
            if let Some(dst) = call_frame.dst {
                let register = self.location(dst).expect("location not found");
                *register = value;
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
    pub fn current_closure(&self) -> Option<&Closure> {
        self.current_call_frame()
            .map(|call_frame| call_frame.closure.as_ref())
    }
    pub fn current_instr(&self) -> Option<&Located<ByteCode>> {
        let code = &self.current_closure()?.code;
        let ip = self
            .current_call_frame()
            .expect("no call frame on stack")
            .ip;
        code.get(ip)
    }
    pub fn register(&self, register: usize) -> Option<&Value> {
        let stack_base = self
            .current_call_frame()
            .expect("no call frame on stack")
            .stack_base;
        self.stack.get(stack_base + register)
    }
    pub fn register_mut(&mut self, register: usize) -> Option<&mut Value> {
        let stack_base = self
            .current_call_frame()
            .expect("no call frame on stack")
            .stack_base;
        self.stack.get_mut(stack_base + register)
    }
    pub fn constant(&self, addr: usize) -> Option<&Value> {
        let consts = &self.current_closure().expect("no current closure").consts;
        consts.get(addr)
    }
    pub fn source(&self, source: Source) -> Option<&Value> {
        match source {
            Source::Register(register) => self.register(register),
            Source::Const(addr) => self.constant(addr),
            Source::Null => Some(GLOBAL_NULL),
            Source::Global(addr) => {
                let ident = match self.constant(addr)? {
                    Value::String(ident) => ident.clone(),
                    _ => return None
                };
                self.globals.get(ident.as_ref())
            }
        }
    }
    pub fn location(&mut self, location: Location) -> Option<&mut Value> {
        match location {
            Location::Register(register) => self.register_mut(register),
            Location::Global(addr) => {
                let ident = match self.constant(addr)? {
                    Value::String(ident) => ident.clone(),
                    _ => return None
                };
                self.globals.get_mut(ident.as_ref())
            }
        }
    }

    pub fn run(&mut self, closure: Closure) -> Result<(), Located<RunTimeError>> {
        self.enter_call(Rc::new(closure), vec![], None);
        loop {
            if self.step()? {
                break;
            }
        }
        Ok(())
    }
    pub fn step(&mut self) -> Result<bool, Located<RunTimeError>> {
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
                if bool::from(cond) && !not {
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
                let func = self.source(func).expect("source not found").clone();
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
                            let value = func(self, args)?;
                            if let Some(dst) = dst {
                                let register = self.location(dst).expect("location not found");
                                *register = value.unwrap_or_default();
                            }
                        }
                    }
                    value => return Err(Located::new(RunTimeError::NotCallable(value), pos)),
                }
            }
            ByteCode::Return { src } => {
                let return_value = self.source(src).expect("source not found").clone();
                self.return_call(return_value);
                if self.call_stack.is_empty() {
                    return Ok(true)
                }
            }
            ByteCode::Null { dst } => {
                let register = self.location(dst).expect("location not found");
                *register = Value::Null;
            }
            ByteCode::Move { dst, src } => {
                let value = self.source(src).expect("source not found").clone();
                let register = self.location(dst).expect("location not found");
                *register = value;
            }
            ByteCode::Binary {
                op,
                dst,
                left,
                right,
            } => {
                let left = self.source(left).expect("source not found").clone();
                let right = self.source(right).expect("source not found").clone();
                let register = self.location(dst).expect("location not found");
                *register = match op {
                    BinaryOperator::Add => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Int(left + right),
                        (Value::Float(left), Value::Float(right)) => Value::Float(left + right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Float(left as f32 + right)
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Float(left + right as f32)
                        }
                        (left, right) => {
                            return Err(Located::new(
                                RunTimeError::Binary(op, left, right),
                                pos.clone(),
                            ))
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
                            return Err(Located::new(
                                RunTimeError::Binary(op, left, right),
                                pos.clone(),
                            ))
                        }
                    },
                    BinaryOperator::Div => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Int(left / right),
                        (Value::Float(left), Value::Float(right)) => Value::Float(left / right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Float(left as f32 / right)
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Float(left / right as f32)
                        }
                        (left, right) => {
                            return Err(Located::new(
                                RunTimeError::Binary(op, left, right),
                                pos.clone(),
                            ))
                        }
                    },
                    BinaryOperator::Mul => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Int(left / right),
                        (Value::Float(left), Value::Float(right)) => Value::Float(left / right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Float(left as f32 / right)
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Float(left / right as f32)
                        }
                        (left, right) => {
                            return Err(Located::new(
                                RunTimeError::Binary(op, left, right),
                                pos.clone(),
                            ))
                        }
                    },
                };
            }
            ByteCode::Unary { op, dst, right } => {
                let right = self.source(right).expect("source not found").clone();
                let register = self.location(dst).expect("location not found");
                *register = match op {
                    UnaryOperator::Neg => match right {
                        Value::Int(right) => Value::Int(-right),
                        Value::Float(right) => Value::Float(-right),
                        right => {
                            return Err(Located::new(RunTimeError::Unary(op, right), pos.clone()))
                        }
                    },
                };
            }
            ByteCode::Field { dst, head, field } => {
                let head = self.source(head).expect("source not found").clone();
                let field = self.source(field).expect("source not found").clone();
                let register = self.location(dst).expect("location not found");
                *register = match &head {
                    Value::Object(object) => match field {
                        Value::String(field) => object.map.get(field.as_str()).cloned().unwrap_or_default(),
                        field => return Err(Located::new(RunTimeError::InvalidField(head, field), pos))
                    }
                    Value::Vector(vector) => match field {
                        Value::Int(index) => vector.get(index as u32 as usize).cloned().unwrap_or_default(),
                        field => return Err(Located::new(RunTimeError::InvalidField(head, field), pos))
                    }
                    _ => return Err(Located::new(RunTimeError::InvalidFieldHead(head), pos))
                };
            }
        }
        Ok(false)
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
impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Int(v) => write!(f, "{v}"),
            Value::Float(v) => write!(f, "{v}"),
            Value::Bool(v) => write!(f, "{v}"),
            Value::String(string) => write!(f, "{string}"),
            Value::Vector(vector) => write!(f, "{vector:?}"),
            Value::Object(object) => write!(f, "object:{:?}", object.as_ref() as *const Object),
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
            Value::Vector(vector) => write!(f, "{vector:?}"),
            Value::Object(object) => write!(f, "object:{:?}", object.as_ref() as *const Object),
            Value::Function(kind) => write!(f, "function:{kind}"),
        }
    }
}
impl Display for FunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionKind::Function(closure) => write!(f, "{:?}", closure.as_ref() as *const Closure),
            FunctionKind::NativeFunction(func) => write!(f, "{:?}", func as *const NativeFunction),
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
            Value::Vector(vector) => !vector.is_empty(),
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
        Self::String(Rc::new(value))
    }
}
impl From<HashMap<String, Value>> for Value {
    fn from(value: HashMap<String, Value>) -> Self {
        Self::Object(Rc::new(Object::from(value)))
    }
}
impl From<Closure> for Value {
    fn from(value: Closure) -> Self {
        Self::Function(FunctionKind::Function(Rc::new(value)))
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
        Self::Vector(Rc::new(
            value.into_iter().map(|value| value.into()).collect(),
        ))
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
    CantGetInnerValue(Rc<T>),
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
            Value::String(value) => match Rc::try_unwrap(value) {
                Ok(value) => Ok(value),
                Err(err) => Err(TryFromValueRcError::CantGetInnerValue(err)),
            },
            _ => Err(TryFromValueRcError::MissmatchedValue),
        }
    }
}
impl TryFrom<Value> for Vec<Value> {
    type Error = TryFromValueRcError<Self>;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Vector(value) => match Rc::try_unwrap(value) {
                Ok(value) => Ok(value),
                Err(err) => Err(TryFromValueRcError::CantGetInnerValue(err)),
            },
            _ => Err(TryFromValueRcError::MissmatchedValue),
        }
    }
}
impl TryFrom<Value> for Object {
    type Error = TryFromValueRcError<Self>;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Object(value) => match Rc::try_unwrap(value) {
                Ok(value) => Ok(value),
                Err(err) => Err(TryFromValueRcError::CantGetInnerValue(err)),
            },
            _ => Err(TryFromValueRcError::MissmatchedValue),
        }
    }
}

impl Display for RunTimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunTimeError::Binary(op, left, right) => {
                write!(f, "cannot perform {op:?} on {left:?} with {right:?}")
            }
            RunTimeError::Unary(op, right) => write!(f, "cannot perform {op:?} on {right:?}"),
            RunTimeError::NotCallable(value) => write!(f, "cannot call {value:?}"),
            RunTimeError::InvalidFieldHead(head) => write!(f, "cannot get field of {head:?}"),
            RunTimeError::InvalidField(head, field) => write!(f, "cannot get field of {head:?} with {field:?}"),
            
        }
    }
}
impl Error for RunTimeError {}
