use std::fmt;
use std::fmt::Formatter;
use crate::lang::{Position, Token};

pub enum SyntaxErrorKind {
    MalformedNumber(String),
    UnknownCharacter(char),
    UnfinishedString,
    UnfinishedMultilineComment,
    ExpectedXButGotY { expected: Token, got: Token },
    ExpectedIdentifierButGotX(Token),
    UnexpectedToken(Token),
    DuplicateFunctionDefinition(String),
    MissingMainFunction,
    UnresolvedName(String),
}

impl fmt::Display for SyntaxErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SyntaxErrorKind::MalformedNumber(s) => {
                write!(f, "Malformed number: '{}'", s)
            },
            SyntaxErrorKind::UnknownCharacter(c) => {
                write!(f, "Unknown character: '{}'", c)
            }
            SyntaxErrorKind::UnfinishedString => {
                write!(f, "Unfinished string")
            }
            SyntaxErrorKind::UnfinishedMultilineComment => {
                write!(f, "Unfinished multi-line comment")
            }
            SyntaxErrorKind::ExpectedXButGotY { expected, got } => {
                write!(f, "Expected '{}' but got '{}'", expected, got)
            }
            SyntaxErrorKind::ExpectedIdentifierButGotX(got) => {
                write!(f, "Expected identifier but got '{}'", got)
            }
            SyntaxErrorKind::UnexpectedToken(token) => {
                write!(f, "Did not expect '{}' here", token)
            }
            SyntaxErrorKind::DuplicateFunctionDefinition(name) => {
                write!(f, "Function '{}' defined more than once", name)
            }
            SyntaxErrorKind::MissingMainFunction => {
                write!(f, "Program is missing main function")
            }
            SyntaxErrorKind::UnresolvedName(name) => {
                write!(f, "Name not found: '{}'", name)
            }
        }
    }
}

pub struct SyntaxError {
    pub error: SyntaxErrorKind,
    pub position: Position
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}: {}", self.position.source_name, self.position.line + 1, self.position.column, self.error)
    }
}