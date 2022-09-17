use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Copy, Clone, strum::Display, strum::EnumString)]
#[strum(serialize_all = "lowercase")]
pub enum Keyword {
    If,
    ElseIf,
    Else,
    While,
    Return,

    True,
    False,
    Nil,

    Func,
    Var,
}

#[derive(Debug, PartialEq, Copy, Clone, strum::Display, strum::EnumString)]
pub enum Punctuation {
    #[strum(serialize = "+")]
    Plus,
    #[strum(serialize = "-")]
    Minus,
    #[strum(serialize = "*")]
    Asterisk,
    #[strum(serialize = "/")]
    Slash,
    #[strum(serialize = "=")]
    Assign,
    #[strum(serialize = "==")]
    Equal,
    #[strum(serialize = "!=")]
    NotEqual,
    #[strum(serialize = ">")]
    Greater,
    #[strum(serialize = "<")]
    Less,
    #[strum(serialize = ">=")]
    GEqual,
    #[strum(serialize = "<=")]
    LEqual,
    #[strum(serialize = "&")]
    And,
    #[strum(serialize = "|")]
    Or,
    #[strum(serialize = "&&")]
    BooleanAnd,
    #[strum(serialize = "||")]
    BooleanOr,
    #[strum(serialize = "!")]
    Exclamation,
    #[strum(serialize = "..")]
    Concat,
    #[strum(serialize = ".")]
    Dot,
    #[strum(serialize = ",")]
    Comma,
    #[strum(serialize = "(")]
    LParen,
    #[strum(serialize = ")")]
    RParen,
    #[strum(serialize = "[")]
    LSquare,
    #[strum(serialize = "]")]
    RSquare,
    #[strum(serialize = "{")]
    LCurly,
    #[strum(serialize = "}")]
    RCurly,
    #[strum(serialize = ";")]
    Semicolon,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    EOF,
    Number(f64),
    Identifier(String),
    String(String),
    Keyword(Keyword),
    Punctuation(Punctuation),
}
impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::EOF => {
                write!(f, "<EOF>")
            }
            Token::Number(n) => {
                write!(f, "{}", n)
            }
            Token::Identifier(i) => {
                write!(f, "{}", i)
            }
            Token::String(s) => {
                write!(f, "{}", s)
            }
            Token::Keyword(k) => {
                write!(f, "{}", k)
            }
            Token::Punctuation(p) => {
                write!(f, "{}", p)
            }
        }
    }
}
