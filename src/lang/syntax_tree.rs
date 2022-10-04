use std::cell::{RefCell};
use std::ops::Deref;

use crate::lang::{ToBytecode, Token};
use crate::lang::token::Punctuation;
use crate::vm::Bytecode;

use super::Position;

#[derive(Debug, PartialEq)]
pub enum Associativity {
    Left,
    Right
}
#[derive(Debug, Copy, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    NotEqual,
    Less,
    Greater,
    LEqual,
    GEqual,
    BooleanAnd,
    BooleanOr,
    Concat,
}
impl BinaryOperator {
    pub fn get_operator(token: &Token) -> Option<BinaryOperator> {
        match token {
            Token::Punctuation(Punctuation::Plus) => Some(BinaryOperator::Add),
            Token::Punctuation(Punctuation::Minus) => Some(BinaryOperator::Sub),
            Token::Punctuation(Punctuation::Asterisk) => Some(BinaryOperator::Mul),
            Token::Punctuation(Punctuation::Slash) => Some(BinaryOperator::Div),
            Token::Punctuation(Punctuation::Equal) => Some(BinaryOperator::Equal),
            Token::Punctuation(Punctuation::NotEqual) => Some(BinaryOperator::NotEqual),
            Token::Punctuation(Punctuation::Greater) => Some(BinaryOperator::Greater),
            Token::Punctuation(Punctuation::Less) => Some(BinaryOperator::Less),
            Token::Punctuation(Punctuation::GEqual) => Some(BinaryOperator::GEqual),
            Token::Punctuation(Punctuation::LEqual) => Some(BinaryOperator::LEqual),
            Token::Punctuation(Punctuation::BooleanAnd) => Some(BinaryOperator::BooleanAnd),
            Token::Punctuation(Punctuation::BooleanOr) => Some(BinaryOperator::BooleanOr),
            Token::Punctuation(Punctuation::Concat) => Some(BinaryOperator::Concat),
            _ => None
        }
    }

    pub fn precedence(&self) -> i32 {
        match self {
            BinaryOperator::BooleanAnd | BinaryOperator::BooleanOr => 0,

            BinaryOperator::Equal | BinaryOperator::NotEqual |
            BinaryOperator::Less | BinaryOperator::Greater |
            BinaryOperator::LEqual | BinaryOperator::GEqual => 1,

            BinaryOperator::Add | BinaryOperator::Sub => 2,

            BinaryOperator::Mul | BinaryOperator::Div => 3,

            BinaryOperator::Concat => 4,
        }
    }
    pub fn associativity(&self) -> Associativity {
        match self {
            _ => Associativity::Left
        }
    }
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negate,
    Not
}
impl UnaryOperator {
    pub fn get_operator(p: Punctuation) -> Option<UnaryOperator> {
        match p {
            Punctuation::Minus => Some(UnaryOperator::Negate),
            Punctuation::Exclamation => Some(UnaryOperator::Not),
            _ => None
        }
    }
}

#[derive(strum::Display, PartialEq)]
pub enum Type {
    Number,
    String,
    Boolean
}

#[derive(Debug)]
pub struct IfPart {
    pub condition: Box<Tree>,
    pub body: Box<Tree>,
}

#[derive(Clone, Debug)]
pub struct IdentifierPosition {
    pub name: String,
    pub position: Position,
}

/// An identifier that could not be resolved during the parsing
/// stage, so it's left to be resolved in the linker stage.
/// 
/// It might stay unresolved even after that, in which case it's
/// an error.
#[derive(Clone, Debug)]
pub enum LateReference {
    Unresolved(IdentifierPosition),
    Resolved(Box<Binding>),
}

/// The meaning of an identifier in a certain context -
/// what it refers to
#[derive(Clone, Debug)]
pub enum Binding {
    /// Builtin function
    Builtin(usize),
    /// Local variable
    Local(usize),
    /// User function
    Function(usize),
    /// A reference that is yet to be resolved.
    LateReference(RefCell<LateReference>),
}
impl ToBytecode for Binding {
    fn get_bytecode(&self) -> Vec<Bytecode> {
        match self {
            Binding::Builtin(index) => vec![Bytecode::PushBuiltin(*index)],
            Binding::Local(index) => vec![Bytecode::PushLocal(*index)],
            Binding::Function(index) => vec![Bytecode::PushFunction(*index)],
            // panics here because unresolved references should be caught by the linker
            Binding::LateReference(reference) => {
                match reference.borrow().deref() {
                    LateReference::Unresolved(_) => panic!("Unresolved reference"),
                    LateReference::Resolved(binding) => binding.get_bytecode(),
                }
            },
        }
    }
}

#[derive(Debug)]
pub enum StringLiteral {
    NotIndexed(String),
    Indexed(usize),
}

#[derive(Debug)]
pub enum Tree {
    NumberValue(f64),

    StringLiteral(RefCell<StringLiteral>),

    BoolValue(bool),

    NilValue,

    BindingValue(Binding),

    BinaryOp {
        operator: BinaryOperator,
        lhs: Box<Tree>,
        rhs: Box<Tree>
    },

    UnaryOp {
        operator: UnaryOperator,
        expression: Box<Tree>
    },

    CreateTable {
        init_values: Vec<(Box<Tree>, Box<Tree>)>
    },

    ObjectIndex {
        object: Box<Tree>,
        index: Box<Tree>,
    },

    IfTree {
        true_part: IfPart,
        elseifs: Vec<IfPart>,
        else_body: Option<Box<Tree>>
    },

    WhileTree {
        condition: Box<Tree>,
        body: Box<Tree>
    },

    FunctionCall {
        function: Box<Tree>,
        parameters: Vec<Box<Tree>>,
        is_expression: bool,
    },

    Return {
        value: Option<Box<Tree>>
    },

    Assignment {
        target: Box<Tree>,
        value: Box<Tree>
    },

    Block {
        statements: Vec<Tree>,
        stack_size: usize
    },
}

pub trait TraverseTree {
    fn traverse<F>(&self, f: &mut F) where
    F: FnMut(&Tree);
}

impl TraverseTree for Tree {
    fn traverse<F>(&self, f: &mut F) where
    F: FnMut(&Tree) {
        f(self);
        match self {
            Tree::BinaryOp { lhs, rhs, .. } => {
                lhs.traverse(f);
                rhs.traverse(f);
            },
            Tree::UnaryOp { expression, .. } => {
                expression.traverse(f);
            },
            Tree::CreateTable { init_values } => {
                for v in init_values {
                    v.0.traverse(f);
                    v.1.traverse(f);
                }
            },
            Tree::ObjectIndex { object, index } => {
                object.traverse(f);
                index.traverse(f);
            },
            Tree::IfTree { true_part, elseifs, else_body } => {
                true_part.traverse(f);
                for p in elseifs {
                    p.traverse(f);
                }
                else_body.traverse(f);
            },
            Tree::WhileTree { condition, body } => {
                condition.traverse(f);
                body.traverse(f);
            },
            Tree::FunctionCall { function, parameters, .. } => {
                function.traverse(f);
                parameters.traverse(f);
            },
            Tree::Return { value } => {
                value.traverse(f);
            },
            Tree::Assignment { target, value } => {
                target.traverse(f);
                value.traverse(f);
            },
            Tree::Block { statements, .. } => {
                statements.traverse(f);
            },
            _ => {},
        }
    }
}

impl TraverseTree for Option<Tree> {
    fn traverse<F>(&self, f: &mut F) where
    F: FnMut(&Tree) {
        if let Some(t) = self {
            t.traverse(f);
        }
    }
}

impl TraverseTree for Option<Box<Tree>> {
    fn traverse<F>(&self, f: &mut F) where
    F: FnMut(&Tree) {
        if let Some(t) = self {
            t.traverse(f);
        }
    }
}

impl TraverseTree for Vec<Tree> {
    fn traverse<F>(&self, f: &mut F) where
    F: FnMut(&Tree) {
        for t in self {
            t.traverse(f);
        }
    }
}

impl TraverseTree for Vec<Box<Tree>> {
    fn traverse<F>(&self, f: &mut F) where
    F: FnMut(&Tree) {
        for t in self {
            t.traverse(f);
        }
    }
}

impl TraverseTree for IfPart {
    fn traverse<F>(&self, f: &mut F) where
    F: FnMut(&Tree) {
        self.condition.traverse(f);
        self.body.traverse(f);
    }
}

pub struct FunctionDef {
    pub name: String,
    pub params: Vec<String>,
    pub body: Tree,
    pub stack_size: usize,
    pub position_defined: Position,
}

pub struct FileTree {
    pub function_defs: Vec<FunctionDef>,
}