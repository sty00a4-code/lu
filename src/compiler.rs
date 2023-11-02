use std::{collections::HashMap, rc::Rc};

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
    pub environment: HashMap<String, Value>,
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
            environment: HashMap::default(),
            closures: vec![Closure::default()],
            scope_stacks: vec![vec![Scope::default()]],
            cp: 0,
        }
    }
}
impl Compiler {
    pub fn with_env(mut self, env: HashMap<String, Value>) -> Self {
        self.environment = env;
        self
    }
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
    pub fn get_local(&mut self, ident: &str) -> Option<Location> {
        let scope_stack = self
            .get_scope_stack()
            .expect("no current scope stack");
        for scope in scope_stack.iter().rev() {
            if scope.has_local(ident) {
                return scope.get_local(ident).map(|addr| Location::Register(*addr))
            }
        }
        if self.environment.contains_key(ident) {
            return Some(Location::Global(self.new_const(Value::String(Rc::new(ident.to_string())))))
        }
        None
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
