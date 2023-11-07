use std::{
    error::Error,
    fmt::Display,
    num::{ParseFloatError, ParseIntError},
};

use oneparse::{lexer::*, position::Located};
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    Null,
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),

    LParan,
    RParan,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Equal,
    Dot,
    Colon,
    Comma,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Exponent,
    Exclamation,
    Ampersand,
    Pipe,
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,

    Let,
    Return,
    Do,
    If,
    Else,
    While,
    For,
    In,
    Break,
    Continue,
    Function,
}
#[derive(Debug, Clone, PartialEq)]
pub enum LexError {
    BadCharacter(char),
    ParseIntError(ParseIntError),
    ParserFloatError(ParseFloatError),
    UnclosedString,
}

impl Token {
    pub fn ident(ident: String) -> Self {
        match ident.as_str() {
            "null" => Self::Null,
            "true" => Self::Bool(true),
            "false" => Self::Bool(false),
            "let" => Self::Let,
            "return" => Self::Return,
            "do" => Self::Do,
            "if" => Self::If,
            "else" => Self::Else,
            "while" => Self::While,
            "for" => Self::For,
            "in" => Self::In,
            "break" => Self::Break,
            "continue" => Self::Continue,
            "function" => Self::Function,
            _ => Self::Ident(ident),
        }
    }
    pub fn name(&self) -> String {
        match self {
            Token::Ident(_) => "ident".to_string(),
            Token::Int(_) => "int".to_string(),
            Token::Float(_) => "float".to_string(),
            Token::Bool(_) => "bool".to_string(),
            Token::String(_) => "string".to_string(),
            _ => format!("{:?}", self.to_string()),
        }
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Ident(ident) => write!(f, "{ident}"),
            Token::Null => write!(f, "null"),
            Token::Int(v) => write!(f, "{v:?}"),
            Token::Float(v) => write!(f, "{v:?}"),
            Token::Bool(v) => write!(f, "{v:?}"),
            Token::String(v) => write!(f, "{v:?}"),
            Token::LParan => write!(f, "("),
            Token::RParan => write!(f, ")"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Equal => write!(f, "="),
            Token::Dot => write!(f, "."),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Exponent => write!(f, "^"),
            Token::Exclamation => write!(f, "!"),
            Token::Ampersand => write!(f, "&"),
            Token::Pipe => write!(f, "|"),
            Token::EQ => write!(f, "=="),
            Token::NE => write!(f, "!="),
            Token::LT => write!(f, "<"),
            Token::GT => write!(f, ">"),
            Token::LE => write!(f, "<="),
            Token::GE => write!(f, ">="),
            Token::Let => write!(f, "let"),
            Token::Return => write!(f, "return"),
            Token::Do => write!(f, "do"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::While => write!(f, "while"),
            Token::For => write!(f, "for"),
            Token::In => write!(f, "in"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::Function => write!(f, "function"),
        }
    }
}
impl Lexable for Token {
    type Error = LexError;
    fn lex(lexer: &mut Lexer) -> Result<Option<Located<Self>>, Located<Self::Error>> {
        while let Some(c) = lexer.get() {
            if !c.is_ascii_whitespace() {
                break;
            }
            lexer.advance();
        }
        let mut pos = lexer.pos();
        let Some(c) = lexer.get() else {
            return Ok(None);
        };
        lexer.advance();
        match c {
            '(' => Ok(Some(Located::new(Token::LParan, pos))),
            ')' => Ok(Some(Located::new(Token::RParan, pos))),
            '[' => Ok(Some(Located::new(Token::LBracket, pos))),
            ']' => Ok(Some(Located::new(Token::RBracket, pos))),
            '{' => Ok(Some(Located::new(Token::LBrace, pos))),
            '}' => Ok(Some(Located::new(Token::RBrace, pos))),
            '=' => {
                if lexer.get() == Some('=') {
                    pos.extend(&lexer.pos());
                    lexer.advance();
                    Ok(Some(Located::new(Token::EQ, pos)))
                } else {
                    Ok(Some(Located::new(Token::Equal, pos)))
                }
            }
            '.' => Ok(Some(Located::new(Token::Dot, pos))),
            ':' => Ok(Some(Located::new(Token::Colon, pos))),
            ',' => Ok(Some(Located::new(Token::Comma, pos))),
            '+' => Ok(Some(Located::new(Token::Plus, pos))),
            '-' => Ok(Some(Located::new(Token::Minus, pos))),
            '*' => Ok(Some(Located::new(Token::Star, pos))),
            '/' => Ok(Some(Located::new(Token::Slash, pos))),
            '%' => Ok(Some(Located::new(Token::Percent, pos))),
            '^' => Ok(Some(Located::new(Token::Exponent, pos))),
            '!' => {
                if lexer.get() == Some('=') {
                    pos.extend(&lexer.pos());
                    lexer.advance();
                    Ok(Some(Located::new(Token::NE, pos)))
                } else {
                    Ok(Some(Located::new(Token::Exclamation, pos)))
                }
            }
            '&' => Ok(Some(Located::new(Token::Ampersand, pos))),
            '|' => Ok(Some(Located::new(Token::Pipe, pos))),
            '<' => {
                if lexer.get() == Some('=') {
                    pos.extend(&lexer.pos());
                    lexer.advance();
                    Ok(Some(Located::new(Token::LE, pos)))
                } else {
                    Ok(Some(Located::new(Token::LT, pos)))
                }
            }
            '>' => {
                if lexer.get() == Some('=') {
                    pos.extend(&lexer.pos());
                    lexer.advance();
                    Ok(Some(Located::new(Token::GE, pos)))
                } else {
                    Ok(Some(Located::new(Token::GT, pos)))
                }
            }
            '"' | '\'' => {
                let end_c = c;
                let mut string = String::new();
                while let Some(c) = lexer.get() {
                    if c == end_c {
                        break;
                    }
                    string.push(if c == '\\' {
                        lexer.advance();
                        let Some(esc_c) = lexer.get() else {
                            return Err(Located::new(LexError::UnclosedString, pos))
                        };
                        match esc_c {
                            'n' => '\n',
                            't' => '\t',
                            'r' => '\r',
                            '0' => '\0',
                            esc_c => esc_c
                        }
                    } else {
                        c
                    });
                    lexer.advance();
                }
                pos.extend(&lexer.pos());
                if lexer.get() != Some(end_c) {
                    return Err(Located::new(LexError::UnclosedString, pos));
                }
                lexer.advance();
                Ok(Some(Located::new(Token::String(string), pos)))
            }
            c if c.is_ascii_digit() => {
                let mut number = String::from(c);
                while let Some(c) = lexer.get() {
                    if !c.is_ascii_alphanumeric() {
                        break;
                    }
                    number.push(c);
                    pos.extend(&lexer.pos());
                    lexer.advance();
                }
                if lexer.get() == Some('.') {
                    number.push('.');
                    pos.extend(&lexer.pos());
                    lexer.advance();
                    while let Some(c) = lexer.get() {
                        if !c.is_ascii_alphanumeric() {
                            break;
                        }
                        number.push(c);
                        pos.extend(&lexer.pos());
                        lexer.advance();
                    }
                    match number.parse() {
                        Ok(number) => Ok(Some(Located::new(Token::Float(number), pos))),
                        Err(err) => Err(Located::new(LexError::ParserFloatError(err), pos)),
                    }
                } else {
                    match number.parse() {
                        Ok(number) => Ok(Some(Located::new(Token::Int(number), pos))),
                        Err(err) => Err(Located::new(LexError::ParseIntError(err), pos)),
                    }
                }
            }
            c if c.is_ascii_alphanumeric() || c == '_' => {
                let mut ident = String::from(c);
                while let Some(c) = lexer.get() {
                    if !c.is_ascii_alphanumeric() && c != '_' {
                        break;
                    }
                    ident.push(c);
                    pos.extend(&lexer.pos());
                    lexer.advance();
                }
                Ok(Some(Located::new(Token::ident(ident), pos)))
            }
            c => Err(Located::new(LexError::BadCharacter(c), pos)),
        }
    }
}

impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::BadCharacter(c) => write!(f, "bad character {c:?}"),
            LexError::ParseIntError(err) => write!(f, "{err}"),
            LexError::ParserFloatError(err) => write!(f, "{err}"),
            LexError::UnclosedString => write!(f, "unclosed string"),
        }
    }
}
impl Error for LexError {}
