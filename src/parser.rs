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
        ident: Located<Ident>,
        expr: Located<Expression>,
    },
    Call {
        func: Located<Ident>,
        args: Vec<Located<Expression>>,
    },
    Return(Located<Expression>),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum BinaryOperator {
    Add,
    Sub,
    Div,
    Mul,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum UnaryOperator {
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
}
#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Ident(Ident),
    Null,
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Expression(Box<Located<Expression>>),
}
#[derive(Debug, Clone, PartialEq)]
pub struct Ident(pub String);

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
            let ident = Ident::parse(parser)?;
            let mut pos = ident.pos.clone();
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
                    Ok(Located::new(Statement::Call { func: ident, args }, pos))
                }
                Token::Equal => {
                    let expr = Expression::parse(parser)?;
                    pos.extend(&expr.pos);
                    Ok(Located::new(Self::Assign { ident, expr }, pos))
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
impl Parsable<Token> for Expression {
    type Error = ParserError;
    fn parse(parser: &mut Parser<Token>) -> Result<Located<Self>, Located<Self::Error>> {
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
impl Parsable<Token> for Atom {
    type Error = ParserError;
    fn parse(parser: &mut Parser<Token>) -> Result<Located<Self>, Located<Self::Error>> {
        let Located { value: token, mut pos } = expect!(parser);
        match token {
            Token::Ident(ident) => Ok(Located::new(Self::Ident(Ident(ident)), pos)),
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
            token => Err(Located::new(ParserError::UnexpectedToken(token), pos)),
        }
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
    NotFound(String),
}
impl Compilable for Located<Chunk> {
    type Error = CompileError;
    type Output = ();
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Self::Error>> {
        let Located {
            value: chunk,
            pos,
        } = self;
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
            Statement::Assign { ident: Located { value: ident, pos: ident_pos }, expr } => {
                let src = expr.compile(compiler)?;
                let Some(dst) = compiler.get_local(&ident.0) else {
                    return Err(Located::new(CompileError::NotFound(ident.0), ident_pos))
                };
                compiler.write(ByteCode::Move { dst, src: src.into() }, pos);
                Ok(())
            }
            Statement::Call { func: Located { value: Ident(ident), pos: ident_pos }, args } => {
                let Some(func) = compiler.get_local(&ident) else {
                    return Err(Located::new(CompileError::NotFound(ident), ident_pos))
                };
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
                let right = right.compile(compiler)?;
                let left = left.compile(compiler)?;
                compiler.write(
                    ByteCode::Binary {
                        op,
                        dst: left,
                        left: left.into(),
                        right: right.into(),
                    },
                    pos,
                );
                Ok(left)
            }
            Expression::Unary { op, right } => {
                let right = right.compile(compiler)?;
                compiler.write(
                    ByteCode::Unary {
                        op,
                        dst: right,
                        right: right.into(),
                    },
                    pos,
                );
                Ok(right)
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
        }
    }
}
impl Compilable for Located<Atom> {
    type Error = CompileError;
    type Output = Source;
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Self::Error>> {
        let Located { value: atom, pos } = self;
        match atom {
            Atom::Ident(Ident(ident)) => match compiler.get_local(&ident) {
                Some(location) => Ok(location.into()),
                None => Err(Located::new(CompileError::NotFound(ident), pos)),
            },
            Atom::Null => Ok(Source::Null),
            Atom::Int(v) => Ok(Source::Const(compiler.new_const(Value::Int(v)))),
            Atom::Float(v) => Ok(Source::Const(compiler.new_const(Value::Float(v)))),
            Atom::Bool(v) => Ok(Source::Const(compiler.new_const(Value::Bool(v)))),
            Atom::String(v) => Ok(Source::Const(compiler.new_const(Value::String(Rc::new(v))))),
            Atom::Expression(expr) => Ok((*expr).compile(compiler)?.into()),
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::NotFound(ident) => write!(f, "{ident:?} not found in current scope"),
        }
    }
}
impl Error for CompileError {}
