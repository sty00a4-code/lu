use std::{collections::HashMap, rc::Rc, fmt::Display};

use oneparse::position::{Located, Positon};

use crate::{
    interpreter::Value,
    parser::{BinaryOperator, UnaryOperator},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ByteCode {
    None,
    Halt,

    Jump {
        addr: usize,
    },
    JumpIf {
        cond: Source,
        addr: usize,
        not: bool,
    },

    Call {
        func: Source,
        start: usize,
        amount: usize,
        dst: Option<Location>,
    },
    Return {
        src: Source,
    },

    Move {
        dst: Location,
        src: Source,
    },
    Null {
        dst: Location,
    },
    Vector {
        dst: Location,
        start: usize,
        amount: usize
    },
    Object {
        dst: Location,
    },
    SetField {
        dst: Location,
        field: Source,
        src: Source,
    },

    Binary {
        op: BinaryOperator,
        dst: Location,
        left: Source,
        right: Source,
    },
    Unary {
        op: UnaryOperator,
        dst: Location,
        right: Source,
    },
    Field {
        dst: Location,
        head: Source,
        field: Source
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Source {
    Register(usize),
    Const(usize),
    Null,
    Global(usize),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Location {
    Register(usize),
    Global(usize)
}
#[derive(Debug, Clone, Default)]
pub struct Closure {
    pub code: Vec<Located<ByteCode>>,
    pub consts: Vec<Value>,
    pub registers: usize,
}

pub struct Compiler {
    pub closures: Vec<Closure>,

    scope_stacks: Vec<Vec<Scope>>,
    cp: usize,
}
#[derive(Debug, Clone, Default)]
pub struct Scope {
    locals: HashMap<String, usize>,
    register_base: usize,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            closures: vec![Closure::default()],
            scope_stacks: vec![vec![Scope::default()]],
            cp: 0,
        }
    }
}
impl Compiler {
    pub fn push_closure(&mut self, closure: Closure) {
        self.closures.push(closure);
        self.scope_stacks.push(vec![Scope::default()]);
    }
    pub fn pop_closure(&mut self) -> Option<Closure> {
        self.scope_stacks.push(vec![Scope::default()]);
        self.closures.pop()
    }
    pub fn get_closure(&self) -> Option<&Closure> {
        self.closures.get(self.cp)
    }
    pub fn get_closure_mut(&mut self) -> Option<&mut Closure> {
        self.closures.get_mut(self.cp)
    }
    pub fn get_scope_stack(&self) -> Option<&Vec<Scope>> {
        self.scope_stacks.get(self.cp)
    }
    pub fn get_scope_stack_mut(&mut self) -> Option<&mut Vec<Scope>> {
        self.scope_stacks.get_mut(self.cp)
    }
    pub fn get_scope(&self) -> Option<&Scope> {
        self.get_scope_stack()?.last()
    }
    pub fn get_scope_mut(&mut self) -> Option<&mut Scope> {
        self.get_scope_stack_mut()?.last_mut()
    }
    pub fn write(&mut self, bytecode: ByteCode, pos: Positon) -> usize {
        let closure = self.get_closure_mut().expect("no current closure");
        let addr = closure.code.len();
        closure.code.push(Located::new(bytecode, pos));
        addr
    }
    pub fn overwrite(&mut self, addr: usize, bytecode: ByteCode, pos: Positon) {
        let closure = self.get_closure_mut().expect("no current closure");
        *closure.code.get_mut(addr).expect("no instr at addr") = Located::new(bytecode, pos);
    }
    pub fn new_const(&mut self, value: Value) -> usize {
        let closure = self.get_closure_mut().expect("no current closure");
        for (addr, const_value) in closure.consts.iter().enumerate() {
            if const_value == &value {
                return addr;
            }
        }
        let addr = closure.consts.len();
        closure.consts.push(value);
        addr
    }
    pub fn new_register(&mut self) -> usize {
        let closure = self.get_closure_mut().expect("no current closure");
        let register = closure.registers;
        closure.registers += 1;
        register
    }
    pub fn push_scope(&mut self) {
        let registers = self.get_closure().expect("no current closure").registers;
        let scope_stack = self
            .get_scope_stack_mut()
            .expect("no current scope stack");
        scope_stack.push(Scope { locals: HashMap::new(), register_base: registers });
    }
    pub fn pop_scope(&mut self) {
        let scope_stack = self
            .get_scope_stack_mut()
            .expect("no current scope stack");
        if let Some(scope) = scope_stack.pop() {
            let closure = self.get_closure_mut().expect("no current closure");
            closure.registers = scope.register_base;
        }
    }
    pub fn new_local(&mut self, ident: String) -> usize {
        let register = self.new_register();
        let scope_stack = self
            .get_scope_stack_mut()
            .expect("no current scope stack");
        let scope = scope_stack
            .last_mut()
            .expect("no scope on current scope stack");
        scope.new_local(ident, register);
        register
    }
    pub fn get_local(&mut self, ident: &str) -> Location {
        let scope_stack = self
            .get_scope_stack()
            .expect("no current scope stack");
        for scope in scope_stack.iter().rev() {
            if let Some(addr) = scope.get_local(ident) {
                return Location::Register(*addr)
            }
        }
        Location::Global(self.new_const(Value::String(Rc::new(ident.to_string()))))
    }
}
impl Scope {
    pub fn has_local(&self, ident: &str) -> bool {
        self.locals.contains_key(ident)
    }
    pub fn get_local(&self, ident: &str) -> Option<&usize> {
        self.locals.get(ident)
    }
    pub fn new_local(&mut self, ident: String, register: usize) -> Option<usize> {
        self.locals.insert(ident, register)
    }
}

pub trait Compilable {
    type Error;
    type Output;
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Self::Error>>;
}

impl From<Location> for Source {
    fn from(val: Location) -> Self {
        match val {
            Location::Register(register) => Source::Register(register),
            Location::Global(addr) => Source::Global(addr),
        }
    }
}
impl From<usize> for Source {
    fn from(value: usize) -> Self {
        Self::Register(value)
    }
}
impl From<Location> for usize {
    fn from(val: Location) -> Self {
        match val {
            Location::Register(register) => register,
            Location::Global(addr) => addr,
        }
    }
}

impl Source {
    pub fn display_code(&self, closure: &Closure) -> String {
        format!("src({})", match self {
            Source::Register(register) => format!("reg@{register}"),
            Source::Const(addr) => format!("const@{addr}={:?}", closure.consts.get(*addr).cloned().unwrap_or_default()),
            Source::Null => Value::Null.to_string(),
            Source::Global(addr) => format!("glob@{addr}={:?}", closure.consts.get(*addr).cloned().unwrap_or_default()),
        })
    } 
}
impl Location {
    pub fn display_code(&self, closure: &Closure) -> String {
        format!("loc({})", match self {
            Location::Register(register) => format!("reg@{register}"),
            Location::Global(addr) => format!("glob@{addr}={:?}", closure.consts.get(*addr).cloned().unwrap_or_default()),
        })
    } 
}
impl Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "closure at {:?}", self as *const Closure)?;
        for (addr, Located { value: bytecode, pos: _ }) in self.code.iter().enumerate() {
            writeln!(f, "\t[{addr}] {}", match bytecode {
                ByteCode::None => "none".to_string(),
                ByteCode::Halt => "halt".to_string(),
                ByteCode::Jump { addr } => format!("jump {addr}"),
                ByteCode::JumpIf { cond, addr, not } => format!("jump {} {addr} not={not}", cond.display_code(self)),
                ByteCode::Call { func, start, amount, dst } => format!("call {} {start} {amount} {:?}", func.display_code(self), dst.map(|loc| loc.display_code(self))),
                ByteCode::Return { src } => format!("return {}", src.display_code(self)),
                ByteCode::Move { dst, src } => format!("move {} {}", dst.display_code(self), src.display_code(self)),
                ByteCode::Null { dst } => format!("null {}", dst.display_code(self)),
                ByteCode::Vector { dst, start, amount } => format!("vector {} {start} {amount}", dst.display_code(self)),
                ByteCode::Object { dst } => format!("object {}", dst.display_code(self)),
                ByteCode::SetField { dst, field, src } => format!("setfield {} {} {}", dst.display_code(self), field.display_code(self), src.display_code(self)),
                ByteCode::Binary { op, dst, left, right } => format!("binary {op:?} {} {} {}", dst.display_code(self), left.display_code(self), right.display_code(self)),
                ByteCode::Unary { op, dst, right } => format!("unary {op:?} {} {}", dst.display_code(self), right.display_code(self)),
                ByteCode::Field { dst, head, field } => format!("field {} {} {}", dst.display_code(self), head.display_code(self), field.display_code(self)),
            })?;
        }

        Ok(())
    }
}