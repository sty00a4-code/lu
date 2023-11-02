use std::{error::Error, fmt::Display, rc::Rc};

use oneparse::{
    parser::{Parsable, Parser},
    position::{Located, Positon},
};

use crate::{
    compiler::{ByteCode, Compilable, Compiler, Location, Source},
    interpreter::Value,
    lexer::Token,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Chunk(pub Vec<Located<Statement>>);
#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<Located<Statement>>);
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {
        ident: Located<Ident>,
        expr: Located<Expression>,
    },
    Assign {
        path: Located<Path>,
        expr: Located<Expression>,
    },
    Call {
        func: Located<Path>,
        args: Vec<Located<Expression>>,
    },
    Return(Located<Expression>),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum BinaryOperator {
    And,
    Or,
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum UnaryOperator {
    Not,
    Neg,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Atom(Atom),
    Binary {
        op: BinaryOperator,
        left: Box<Located<Self>>,
        right: Box<Located<Self>>,
    },
    Unary {
        op: UnaryOperator,
        right: Box<Located<Self>>,
    },
    Call {
        func: Located<Atom>,
        args: Vec<Located<Expression>>,
    },
    Field {
        head: Box<Located<Self>>,
        field: Located<Ident>
    },
    Index {
        head: Box<Located<Self>>,
        index: Box<Located<Expression>>
    },
}
#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Path(Path),
    Null,
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Expression(Box<Located<Expression>>),
    Vector(Vec<Located<Expression>>),
    Object(Vec<(Located<Ident>, Located<Expression>)>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum Path {
    Ident(Ident),
    Field {
        head: Box<Located<Self>>,
        field: Located<Ident>
    },
    Index {
        head: Box<Located<Self>>,
        index: Box<Located<Expression>>
    },
}
#[derive(Debug, Clone, PartialEq)]
pub struct Ident(pub String);

impl<'a> BinaryOperator {
    pub const LAYER: &'a [&'a [Self]] = &[
        &[Self::And, Self::Or],
        &[Self::EQ, Self::NE, Self::LT, Self::GT, Self::LE, Self::GE],
        &[Self::Add, Self::Sub],
        &[Self::Mul, Self::Div, Self::Mod],
        &[Self::Pow],
    ];
    pub fn layer(layer: usize) -> Option<&'a &'a [Self]> {
        Self::LAYER.get(layer)
    }
    pub fn token(token: &Token) -> Option<Self> {
        match token {
            Token::Ampersand => Some(Self::And),
            Token::Pipe => Some(Self::Or),
            Token::EQ => Some(Self::EQ),
            Token::NE => Some(Self::NE),
            Token::LT => Some(Self::LT),
            Token::GT => Some(Self::GT),
            Token::LE => Some(Self::LE),
            Token::GE => Some(Self::GE),
            Token::Plus => Some(Self::Add),
            Token::Minus => Some(Self::Sub),
            Token::Star => Some(Self::Mul),
            Token::Slash => Some(Self::Div),
            Token::Percent => Some(Self::Mod),
            Token::Exponent => Some(Self::Pow),
            _ => None
        }
    }
}
impl<'a> UnaryOperator {
    pub const LAYER: &'a [&'a [Self]] = &[
        &[Self::Not],
        &[Self::Neg],
    ];
    pub fn layer(layer: usize) -> Option<&'a &'a [Self]> {
        Self::LAYER.get(layer)
    }
    pub fn token(token: &Token) -> Option<Self> {
        match token {
            Token::Exclamation => Some(Self::Not),
            Token::Minus => Some(Self::Neg),
            _ => None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParserError {
    UnexpectedEndOfFile,
    UnexpectedToken(Token),
    ExpectedToken { expected: Token, got: Token },
}
macro_rules! expect {
    ($parser:ident) => {{
        let Some(token_loc) = $parser.token() else {
            return Err(Located::new(
                ParserError::UnexpectedEndOfFile,
                Positon::default(),
            ));
        };
        token_loc
    }};
}
macro_rules! expect_token {
    ($parser:ident : $token:ident) => {{
        let token_loc = expect!($parser);
        if token_loc.value != Token::$token {
            return Err(Located::new(
                ParserError::ExpectedToken {
                    expected: Token::$token,
                    got: token_loc.value,
                },
                token_loc.pos,
            ));
        }
        token_loc
    }};
}
impl Parsable<Token> for Chunk {
    type Error = ParserError;
    fn parse(parser: &mut Parser<Token>) -> Result<Located<Self>, Located<Self::Error>> {
        let mut stats = vec![];
        let mut pos = Positon::default();
        while parser.token_ref().is_some() {
            let stat = Statement::parse(parser)?;
            pos.extend(&stat.pos);
            stats.push(stat);
        }
        Ok(Located::new(Self(stats), pos))
    }
}
impl Parsable<Token> for Block {
    type Error = ParserError;
    fn parse(parser: &mut Parser<Token>) -> Result<Located<Self>, Located<Self::Error>> {
        let mut stats = vec![];
        let Located { value: _, mut pos } = expect_token!(parser: LBrace);
        while let Some(Located {
            value: token,
            pos: _,
        }) = parser.token_ref()
        {
            if token == &Token::RBrace {
                break;
            }
            let stat = Statement::parse(parser)?;
            pos.extend(&stat.pos);
            stats.push(stat);
        }
        let Located {
            value: _,
            pos: end_pos,
        } = expect_token!(parser: RBrace);
        pos.extend(&end_pos);
        Ok(Located::new(Self(stats), pos))
    }
}
impl Parsable<Token> for Statement {
    type Error = ParserError;
    fn parse(parser: &mut Parser<Token>) -> Result<Located<Self>, Located<Self::Error>> {
        if let Some(Located {
            value: Token::Ident(_),
            pos: _,
        }) = parser.token_ref()
        {
            let path = Path::parse(parser)?;
            let mut pos = path.pos.clone();
            let Located { value: token, pos: token_pos } = expect!(parser);
            return match token {
                Token::LParan => {
                    let mut args = vec![];
                    while let Some(Located { value: token, pos: _ }) = parser.token_ref() {
                        if token == &Token::RParan {
                            break;
                        }
                        args.push(Expression::parse(parser)?);
                        if let Some(Located { value: Token::RParan, pos: _ }) = parser.token_ref() {
                            break;
                        }
                        expect_token!(parser: Comma);
                    }
                    let Located { value: _, pos: end_pos } = expect_token!(parser: RParan);
                    pos.extend(&end_pos);
                    Ok(Located::new(Statement::Call { func: path, args }, pos))
                }
                Token::Equal => {
                    let expr = Expression::parse(parser)?;
                    pos.extend(&expr.pos);
                    Ok(Located::new(Self::Assign { path, expr }, pos))
                }
                token => Err(Located::new(ParserError::UnexpectedToken(token), token_pos))
            }
        }
        let Located {
            value: token,
            mut pos,
        } = expect!(parser);
        match token {
            Token::Let => {
                let ident = Ident::parse(parser)?;
                expect_token!(parser: Equal);
                let expr = Expression::parse(parser)?;
                pos.extend(&expr.pos);
                Ok(Located::new(Self::Let { ident, expr }, pos))
            }
            Token::Return => {
                let expr = Expression::parse(parser)?;
                pos.extend(&expr.pos);
                Ok(Located::new(Self::Return(expr), pos))
            }
            token => Err(Located::new(ParserError::UnexpectedToken(token), pos)),
        }
    }
}
impl Expression {
    pub fn binary(parser: &mut Parser<Token>, layer: usize) -> Result<Located<Self>, Located<ParserError>> {
        let Some(ops) = BinaryOperator::layer(layer) else {
            return Self::unary(parser, 0)
        };
        let mut left = Self::binary(parser, layer + 1)?;
        while let Some(Located { value: token, pos: _ }) = parser.token_ref() {
            let Some(op) = BinaryOperator::token(token) else {
                break;
            };
            if !ops.contains(&op) {
                break;
            }
            parser.token();
            let mut pos = left.pos.clone();
            let right = Self::binary(parser, layer + 1)?;
            pos.extend(&right.pos);
            left = Located::new(Self::Binary { op, left: Box::new(left), right: Box::new(right) }, pos);
        }
        Ok(left)
    }
    pub fn unary(parser: &mut Parser<Token>, layer: usize) -> Result<Located<Self>, Located<ParserError>> {
        let Some(ops) = UnaryOperator::layer(layer) else {
            return Self::field(parser)
        };
        if let Some(Located { value: token, pos: _ }) = parser.token_ref() {
            if let Some(op) = UnaryOperator::token(token) {
                if ops.contains(&op) {
                    let Located { value: _, mut pos } = parser.token().unwrap();
                    let right = Self::unary(parser, layer)?;
                    pos.extend(&right.pos);
                    return Ok(Located::new(Self::Unary { op, right: Box::new(right) }, pos))
                }
            }
        }
        Self::unary(parser, layer + 1)
    }
    pub fn field(parser: &mut Parser<Token>) -> Result<Located<Self>, Located<ParserError>> {
        let mut head = Self::call(parser)?;
        while let Some(Located { value: token, pos: _ }) = parser.token_ref() {
            match token {
                Token::Dot => {
                    parser.token();
                    let field = Ident::parse(parser)?;
                    let mut pos = head.pos.clone();
                    pos.extend(&field.pos);
                    head = Located::new(Self::Field { head: Box::new(head), field }, pos)
                }
                Token::LBracket => {
                    parser.token();
                    let index = Expression::parse(parser)?;
                    let Located { value: _, pos: end_pos } = expect_token!(parser: RBracket);
                    let mut pos = head.pos.clone();
                    pos.extend(&end_pos);
                    head = Located::new(Self::Index { head: Box::new(head), index: Box::new(index) }, pos)
                }
                _ => break
            }
        }
        Ok(head)
    }
    pub fn call(parser: &mut Parser<Token>) -> Result<Located<Self>, Located<ParserError>> {
        let atom = Atom::parse(parser)?;
        if let Some(Located { value: Token::LParan, pos: _ }) = parser.token_ref() {
            parser.token();
            let mut pos = atom.pos.clone();
            let mut args = vec![];
            while let Some(Located { value: token, pos: _ }) = parser.token_ref() {
                if token == &Token::RParan {
                    break;
                }
                args.push(Expression::parse(parser)?);
                if let Some(Located { value: Token::RParan, pos: _ }) = parser.token_ref() {
                    break;
                }
                expect_token!(parser: Comma);
            }
            let Located { value: _, pos: end_pos } = expect_token!(parser: RParan);
            pos.extend(&end_pos);
            Ok(Located::new(Self::Call { func: atom, args }, pos))
        } else {
            Ok(atom.map(Self::Atom))
        }
    }
}
impl Parsable<Token> for Expression {
    type Error = ParserError;
    fn parse(parser: &mut Parser<Token>) -> Result<Located<Self>, Located<Self::Error>> {
        Self::binary(parser, 0)
    }
}
impl Parsable<Token> for Atom {
    type Error = ParserError;
    fn parse(parser: &mut Parser<Token>) -> Result<Located<Self>, Located<Self::Error>> {
        if let Some(Located { value: Token::Ident(_), pos: _ }) = parser.token_ref() {
            return Ok(Path::parse(parser)?.map(Self::Path))
        }
        let Located { value: token, mut pos } = expect!(parser);
        match token {
            Token::Null => Ok(Located::new(Self::Null, pos)),
            Token::Int(v) => Ok(Located::new(Self::Int(v), pos)),
            Token::Float(v) => Ok(Located::new(Self::Float(v), pos)),
            Token::Bool(v) => Ok(Located::new(Self::Bool(v), pos)),
            Token::String(v) => Ok(Located::new(Self::String(v), pos)),
            Token::LParan => {
                let expr = Expression::parse(parser)?;
                let Located { value: _, pos: end_pos } = expect_token!(parser : RParan);
                pos.extend(&end_pos);
                Ok(Located::new(Self::Expression(Box::new(expr)), pos))
            }
            Token::LBracket => {
                let mut vector = vec![];
                while let Some(Located { value: token, pos: _ }) = parser.token_ref() {
                    if token == &Token::RBracket {
                        break;
                    }
                    vector.push(Expression::parse(parser)?);
                    if let Some(Located { value: Token::RBracket, pos: _ }) = parser.token_ref() {
                        break;
                    }
                    expect_token!(parser: Comma);
                }
                let Located { value: _, pos: end_pos } = expect_token!(parser : RBracket);
                pos.extend(&end_pos);
                Ok(Located::new(Self::Vector(vector), pos))
            }
            Token::LBrace => {
                let mut object = vec![];
                while let Some(Located { value: token, pos: _ }) = parser.token_ref() {
                    if token == &Token::RBrace {
                        break;
                    }
                    let field = Ident::parse(parser)?;
                    expect_token!(parser: Equal);
                    let expr = Expression::parse(parser)?;
                    object.push((field, expr));
                    if let Some(Located { value: Token::RBrace, pos: _ }) = parser.token_ref() {
                        break;
                    }
                    expect_token!(parser: Comma);
                }
                let Located { value: _, pos: end_pos } = expect_token!(parser : RBrace);
                pos.extend(&end_pos);
                Ok(Located::new(Self::Object(object), pos))
            }
            token => Err(Located::new(ParserError::UnexpectedToken(token), pos)),
        }
    }
}
impl Parsable<Token> for Path {
    type Error = ParserError;
    fn parse(parser: &mut Parser<Token>) -> Result<Located<Self>, Located<Self::Error>> {
        let mut head = Ident::parse(parser)?.map(Self::Ident);
        while let Some(Located { value: token, pos: _ }) = parser.token_ref() {
            match token {
                Token::Dot => {
                    parser.token();
                    let field = Ident::parse(parser)?;
                    let mut pos = head.pos.clone();
                    pos.extend(&field.pos);
                    head = Located::new(Self::Field { head: Box::new(head), field }, pos)
                }
                Token::LBracket => {
                    parser.token();
                    let index = Expression::parse(parser)?;
                    let Located { value: _, pos: end_pos } = expect_token!(parser: RBracket);
                    let mut pos = head.pos.clone();
                    pos.extend(&end_pos);
                    head = Located::new(Self::Index { head: Box::new(head), index: Box::new(index) }, pos)
                }
                _ => break,
            }
        }
        Ok(head)
    }
}
impl Parsable<Token> for Ident {
    type Error = ParserError;
    fn parse(parser: &mut Parser<Token>) -> Result<Located<Self>, Located<Self::Error>> {
        let Located { value: token, pos } = expect!(parser);
        if let Token::Ident(ident) = token {
            Ok(Located::new(Ident(ident), pos))
        } else {
            Err(Located::new(ParserError::ExpectedToken { expected: Token::Ident("".to_string()), got: token }, pos))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompileError {
}
impl Compilable for Located<Chunk> {
    type Error = CompileError;
    type Output = ();
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Self::Error>> {
        let Located {
            value: chunk,
            pos,
        } = self;
        compiler.new_const(Value::String(Rc::new("__module".to_string())));
        for stat in chunk.0 {
            stat.compile(compiler)?;
        }
        compiler.write(ByteCode::Halt, pos);
        Ok(())
    }
}
impl Compilable for Located<Block> {
    type Error = CompileError;
    type Output = ();
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Self::Error>> {
        let Located {
            value: block,
            pos: _,
        } = self;
        compiler.push_scope();
        for stat in block.0 {
            stat.compile(compiler)?;
        }
        compiler.pop_scope();
        Ok(())
    }
}
impl Compilable for Located<Statement> {
    type Error = CompileError;
    type Output = ();
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Self::Error>> {
        let Located { value: stat, pos } = self;
        match stat {
            Statement::Let { ident, expr } => {
                let dst = expr.compile(compiler)?;
                let scope = compiler.get_scope_mut().expect("no scope on scope stack");
                scope.new_local(ident.value.0, dst.into());
                Ok(())
            }
            Statement::Assign { path: Located { value: path, pos: _ }, expr } => {
                let src = expr.compile(compiler)?;
                match path {
                    Path::Ident(Ident(ident)) => {
                        let dst = compiler.get_local(&ident);
                        compiler.write(ByteCode::Move { dst, src: src.into() }, pos);
                        Ok(())
                    }
                    Path::Field { head, field: Located { value: Ident(field), pos: _ } } => {
                        let dst = head.compile(compiler)?;
                        let field = Source::Const(compiler.new_const(Value::String(Rc::new(field))));
                        compiler.write(ByteCode::SetField { dst, field, src: src.into() }, pos);
                        Ok(())
                    }
                    Path::Index { head, index } => {
                        let dst = head.compile(compiler)?;
                        let index = index.compile(compiler)?;
                        compiler.write(ByteCode::SetField { dst, field: index.into(), src: src.into() }, pos);
                        Ok(())
                    }
                }
            }
            Statement::Call { func, args } => {
                let func = func.compile(compiler)?;
                let amount = args.len();
                let start = compiler.get_closure().expect("no current closure").registers;
                for _ in start..start+amount {
                    compiler.new_register();
                }
                for (dst, arg) in args.into_iter().enumerate() {
                    let arg_dst = arg.compile(compiler)?;
                    compiler.write(ByteCode::Move { dst: Location::Register(start + dst), src: arg_dst.into() }, pos.clone());
                }
                compiler.write(ByteCode::Call { func: func.into(), start, amount, dst: None }, pos);
                Ok(())
            }
            Statement::Return(expr) => {
                let dst = expr.compile(compiler)?;
                compiler.write(ByteCode::Return { src: dst.into() }, pos);
                Ok(())
            }
        }
    }
}
impl Compilable for Located<Expression> {
    type Error = CompileError;
    type Output = Location;
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Self::Error>> {
        let Located { value: expr, pos } = self;
        let pos_clone = pos.clone();
        match expr {
            Expression::Atom(atom) => match Located::new(atom, pos).compile(compiler)? {
                Source::Register(register) => Ok(Location::Register(register)),
                src => {
                    let dst = compiler.new_register();
                    compiler.write(
                        ByteCode::Move {
                            dst: Location::Register(dst),
                            src,
                        },
                        pos_clone,
                    );
                    Ok(Location::Register(dst))
                }
            },
            Expression::Binary { op, left, right } => {
                let dst = compiler.new_register();
                let right = right.compile(compiler)?;
                let left = left.compile(compiler)?;
                compiler.write(
                    ByteCode::Binary {
                        op,
                        dst: Location::Register(dst),
                        left: left.into(),
                        right: right.into(),
                    },
                    pos,
                );
                Ok(Location::Register(dst))
            }
            Expression::Unary { op, right } => {
                let dst = compiler.new_register();
                let right = right.compile(compiler)?;
                compiler.write(
                    ByteCode::Unary {
                        op,
                        dst: Location::Register(dst),
                        right: right.into(),
                    },
                    pos,
                );
                Ok(Location::Register(dst))
            },
            Expression::Call { func, args } => {
                let func = func.compile(compiler)?;
                let dst = compiler.new_register();
                compiler.write(ByteCode::Move { dst: Location::Register(dst), src: func }, pos.clone());
                let amount = args.len();
                let start = compiler.get_closure().expect("no current closure").registers;
                for _ in start..start+amount {
                    compiler.new_register();
                }
                for (dst, arg) in args.into_iter().enumerate() {
                    let arg_dst = arg.compile(compiler)?;
                    compiler.write(ByteCode::Move { dst: Location::Register(start + dst), src: arg_dst.into() }, pos.clone());
                }
                compiler.write(ByteCode::Call { func, start, amount, dst: Some(Location::Register(dst)) }, pos);
                Ok(Location::Register(dst))
            }
            Expression::Field { head, field: Located { value: Ident(field), pos: _ } } => {
                let dst = compiler.new_register();
                let head = head.compile(compiler)?;
                let field = Source::Const(compiler.new_const(Value::String(Rc::new(field))));
                compiler.write(ByteCode::Field { dst: Location::Register(dst), head: head.into(), field }, pos);
                Ok(Location::Register(dst))
            }
            Expression::Index { head, index } => {
                let dst = compiler.new_register();
                let head = head.compile(compiler)?;
                let index = index.compile(compiler)?;
                compiler.write(ByteCode::Field { dst: Location::Register(dst), head: head.into(), field: index.into() }, pos);
                Ok(Location::Register(dst))
            }
        }
    }
}
impl Compilable for Located<Atom> {
    type Error = CompileError;
    type Output = Source;
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Self::Error>> {
        let Located { value: atom, pos } = self;
        match atom {
            Atom::Path(path) => Ok(Located::new(path, pos).compile(compiler)?.into()),
            Atom::Null => Ok(Source::Null),
            Atom::Int(v) => Ok(Source::Const(compiler.new_const(Value::Int(v)))),
            Atom::Float(v) => Ok(Source::Const(compiler.new_const(Value::Float(v)))),
            Atom::Bool(v) => Ok(Source::Const(compiler.new_const(Value::Bool(v)))),
            Atom::String(v) => Ok(Source::Const(compiler.new_const(Value::String(Rc::new(v))))),
            Atom::Expression(expr) => Ok((*expr).compile(compiler)?.into()),
            Atom::Vector(vector) => {
                let dst = compiler.new_register();
                let amount = vector.len();
                let start = compiler.get_closure().expect("no current closure").registers;
                for _ in start..start+amount {
                    compiler.new_register();
                }
                for (dst, expr) in vector.into_iter().enumerate() {
                    let expr_dst = expr.compile(compiler)?;
                    compiler.write(ByteCode::Move { dst: Location::Register(start + dst), src: expr_dst.into() }, pos.clone());
                }
                compiler.write(ByteCode::Vector { dst: Location::Register(dst), start, amount }, pos);
                Ok(Source::Register(dst))
            }
            Atom::Object(object) => {
                let dst = compiler.new_register();
                compiler.write(ByteCode::Object { dst: Location::Register(dst) }, pos.clone());
                for (Located { value: Ident(field), pos: _ }, expr) in object.into_iter() {
                    let src = expr.compile(compiler)?;
                    let field = Source::Const(compiler.new_const(Value::String(Rc::new(field))));
                    compiler.write(ByteCode::SetField { dst: Location::Register(dst), field, src: src.into() }, pos.clone());
                }
                Ok(Source::Register(dst))
            }
        }
    }
}
impl Compilable for Located<Path> {
    type Error = CompileError;
    type Output = Location;
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Self::Error>> {
        let Located { value: path, pos } = self;
        match path {
            Path::Ident(Ident(ident)) => Ok(compiler.get_local(&ident)),
            Path::Field { head, field: Located { value: Ident(field), pos: _ } } => {
                let dst = compiler.new_register();
                let head = head.compile(compiler)?;
                let field = Source::Const(compiler.new_const(Value::String(Rc::new(field))));
                compiler.write(ByteCode::Field { dst: Location::Register(dst), head: head.into(), field }, pos);
                Ok(Location::Register(dst))
            }
            Path::Index { head, index } => {
                let dst = compiler.new_register();
                let head = head.compile(compiler)?;
                let index = index.compile(compiler)?;
                compiler.write(ByteCode::Field { dst: Location::Register(dst), head: head.into(), field: index.into() }, pos);
                Ok(Location::Register(dst))
            }
        }
    }
}


impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::UnexpectedEndOfFile => write!(f, "unexpected end of file"),
            ParserError::UnexpectedToken(token) => write!(f, "unexpected {}", token.name()),
            ParserError::ExpectedToken { expected, got } => {
                write!(f, "expected {}, got {}", expected.name(), got.name())
            }
        }
    }
}
impl Error for ParserError {}
impl Display for CompileError {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}
impl Error for CompileError {}
