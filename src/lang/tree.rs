use crate::lang::{ToBytecode, Token};
use crate::lang::token::Punctuation;
use crate::vm::Bytecode;

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
    BooleanOr
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

/// The meaning of an identifier in a certain context -
/// what it refers to
#[derive(Copy, Clone, Debug)]
pub enum Binding {
    /// Builtin function
    Builtin(usize),
    /// Local variable
    Local(usize),
    /// User function
    Function(usize),
}
impl ToBytecode for Binding {
    fn get_bytecode(&self) -> Vec<Bytecode> {
        match self {
            Binding::Builtin(index) => vec![Bytecode::PushBuiltin(*index)],
            Binding::Local(index) => vec![Bytecode::PushLocal(*index)],
            Binding::Function(index) => vec![Bytecode::PushFunction(*index)],
        }
    }
}

#[derive(Debug)]
pub enum Tree {
    NumberValue(f64),

    StringLiteralValue(usize),

    BoolValue(bool),

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

impl Tree {
    //pub fn get_type(&self) -> Type {
    //    match self {
    //        Tree::NumberValue(_) => Type::Number,
    //        Tree::StringValue(_) => Type::String,
    //        Tree::BoolValue(_) => Type::Boolean,
    //        Tree::BindingValue(_) => todo!(),
    //        Tree::BinaryOp { operator, .. } => {
    //            match operator {
    //                BinaryOperator::Add | BinaryOperator::Sub |
    //                BinaryOperator::Mul | BinaryOperator::Div => Type::Number,

    //                BinaryOperator::Equal | BinaryOperator::NotEqual | BinaryOperator::Less |
    //                BinaryOperator::Greater | BinaryOperator::LEqual |
    //                BinaryOperator::GEqual | BinaryOperator::BooleanAnd |
    //                BinaryOperator::BooleanOr => Type::Boolean
    //            }
    //        }
    //        Tree::UnaryOp { operator, .. } => {
    //            match operator {
    //                UnaryOperator::Negate => Type::Number,
    //                UnaryOperator::Not => Type::Boolean
    //            }
    //        }
    //        Tree::IfTree { body_true, .. } => {
    //            body_true.get_type()
    //        }
    //    }
    //}
}