use crate::lang::tree::{BinaryOperator, Tree, UnaryOperator};
use crate::vm::Bytecode;

pub trait ToBytecode {
    fn get_bytecode(&self) -> Vec<Bytecode>;
}

impl BinaryOperator {
    pub fn get_bytecode(&self) -> Vec<Bytecode> {
        // any panics here are cases that should have been handled by the parser earlier
        match self {
            BinaryOperator::Add => vec![Bytecode::AddNumbers],
            BinaryOperator::Sub => vec![Bytecode::SubNumbers],
            BinaryOperator::Mul => vec![Bytecode::MulNumbers],
            BinaryOperator::Div => vec![Bytecode::DivNumbers],
            BinaryOperator::Equal => vec![Bytecode::CompareEqual],
            BinaryOperator::NotEqual => vec![Bytecode::CompareEqual, Bytecode::Not],
            BinaryOperator::Less => vec![Bytecode::CompareLess],
            BinaryOperator::Greater => vec![Bytecode::CompareGreater],
            BinaryOperator::LEqual => vec![Bytecode::CompareLEqual],
            BinaryOperator::GEqual => vec![Bytecode::CompareGEqual],
            BinaryOperator::BooleanAnd => vec![Bytecode::BooleanAnd],
            BinaryOperator::BooleanOr => vec![Bytecode::BooleanOr],
        }
    }
}

impl ToBytecode for f64 {
    fn get_bytecode(&self) -> Vec<Bytecode> {
        vec![Bytecode::PushNumber(*self)]
    }
}

impl ToBytecode for String {
    fn get_bytecode(&self) -> Vec<Bytecode> {
        vec![Bytecode::PushString(self.clone())]
    }
}

impl ToBytecode for bool {
    fn get_bytecode(&self) -> Vec<Bytecode> {
        vec![Bytecode::PushBool(*self)]
    }
}

impl ToBytecode for Tree {
    fn get_bytecode(&self) -> Vec<Bytecode> {
        match self {
            Tree::NumberValue(t) => t.get_bytecode(),

            Tree::StringValue(t) => t.get_bytecode(),

            Tree::BoolValue(t) => t.get_bytecode(),

            Tree::BindingValue(t) => t.get_bytecode(),

            Tree::BinaryOp { operator, lhs, rhs } => {
                let mut result: Vec<Bytecode> = vec![];

                let mut code_lhs = lhs.get_bytecode();
                let mut code_rhs = rhs.get_bytecode();

                result.append(&mut code_lhs);
                result.append(&mut code_rhs);
                result.append(&mut operator.get_bytecode());

                result
            }

            Tree::UnaryOp { operator, expression } => {
                let mut result: Vec<Bytecode> = expression.get_bytecode();

                result.push(match operator {
                    UnaryOperator::Negate => Bytecode::Negate,
                    UnaryOperator::Not => Bytecode::Not
                });

                result
            }

            Tree::IfTree { first, elseifs, else_body } => {
                let all_ifs = std::iter::once(first).chain(elseifs);

                let mut result: Vec<Bytecode> = vec![];

                for part in all_ifs {
                    let condition = part.condition.get_bytecode();
                    todo!()
                }

                result
            }
            Tree::FunctionCall { function, parameters } => {
                let mut result = vec![];

                for parameter in parameters {
                    result.append(&mut parameter.get_bytecode());
                }

                result.append(&mut function.get_bytecode());
                result.push(Bytecode::Call { num_args: parameters.len() });

                result
            }
            Tree::Assignment { target, value } => {
                todo!()
            }
            Tree::Block(statements) => {
                let mut result = vec![];

                for statement in statements {
                    result.append(&mut statement.get_bytecode());
                }

                result
            }
        }
    }
}