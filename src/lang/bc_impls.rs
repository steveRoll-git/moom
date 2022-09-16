use crate::lang::tree::{BinaryOperator, Tree, UnaryOperator, Binding};
use crate::vm::Bytecode;

use super::tree::IfPart;

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
            BinaryOperator::Concat => vec![Bytecode::Concat],
        }
    }
}

impl ToBytecode for f64 {
    fn get_bytecode(&self) -> Vec<Bytecode> {
        vec![Bytecode::PushNumber(*self)]
    }
}

impl ToBytecode for bool {
    fn get_bytecode(&self) -> Vec<Bytecode> {
        vec![Bytecode::PushBool(*self)]
    }
}

impl ToBytecode for Vec<Tree> {
    fn get_bytecode(&self) -> Vec<Bytecode> {
        let mut result = vec![];

        for statement in self {
            result.append(&mut statement.get_bytecode());
        }

        result
    }
}

impl ToBytecode for Tree {
    fn get_bytecode(&self) -> Vec<Bytecode> {
        match self {
            Tree::NumberValue(t) => t.get_bytecode(),

            Tree::StringLiteralValue(t) => vec![Bytecode::PushStringLiteral(*t)],

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

            Tree::CreateTable { init_values } => {
                let mut result = vec![Bytecode::CreateTable];

                for (index, value) in init_values {
                    result.append(&mut index.get_bytecode());
                    result.append(&mut value.get_bytecode());
                    result.push(Bytecode::SetTable { keep_table: true });  
                }
                
                result
            }

            Tree::ObjectIndex { object, index } => {
                let mut result = vec![];

                result.append(&mut object.get_bytecode());
                result.append(&mut index.get_bytecode());
                result.push(Bytecode::GetTable);

                result
            }

            Tree::IfTree { true_part, elseifs, else_body } => {
                let all_ifs: Vec<&IfPart> = std::iter::once(true_part).chain(elseifs).collect();

                let mut result: Vec<Bytecode> = vec![];

                let mut final_points: Vec<usize> = vec![];

                for (index, part) in all_ifs.iter().enumerate() {
                    let not_last = index < all_ifs.len() - 0 || matches!(else_body, Some(..));

                    let mut condition = part.condition.get_bytecode();
                    let mut body = part.body.get_bytecode();

                    result.append(&mut condition);

                    result.push(Bytecode::JumpIfFalse(body.len() as isize + if not_last { 2 } else { 1 }));

                    result.append(&mut body);

                    if not_last {
                        // this specific instruction is temporary and will be replaced later
                        final_points.push(result.len());
                        result.push(Bytecode::Jump(0));
                    }
                }

                if let Some(body) = else_body {
                    result.append(&mut body.get_bytecode());
                }

                for index in final_points {
                    result[index] = Bytecode::Jump((result.len() - index) as isize);
                }

                result
            }

            Tree::WhileTree { condition, body } => {
                let mut result = vec![];
                let mut body = body.get_bytecode();

                result.append(&mut condition.get_bytecode());
                result.push(Bytecode::JumpIfFalse((body.len() + 2) as isize));
                result.append(&mut body);
                result.push(Bytecode::Jump(-(result.len() as isize)));

                result
            }

            Tree::FunctionCall { function, parameters, is_expression } => {
                let mut result = vec![];

                for parameter in parameters {
                    result.append(&mut parameter.get_bytecode());
                }

                result.append(&mut function.get_bytecode());
                result.push(Bytecode::Call { num_args: parameters.len() });

                if *is_expression {
                    result.push(Bytecode::PushReturn);
                }

                result
            }

            Tree::Return { value } => {
                if let Some(value) = value {
                    let mut result = value.get_bytecode();
                    result.push(Bytecode::Return(true));
                    result
                } else {
                    vec![Bytecode::Return(false)]
                }
            }

            Tree::Assignment { target, value } => {
                let mut result = vec![];

                match target.as_ref() {
                    Tree::BindingValue(binding) => {
                        match binding {
                            Binding::Local(index) => {
                                result.append(&mut value.get_bytecode());
                                result.push(Bytecode::SetLocal(*index))
                            },
                            _ => panic!("Cannot assign to this kind of value")
                        }
                    },
                    Tree::ObjectIndex { object, index } => {
                        result.append(&mut object.get_bytecode());
                        result.append(&mut index.get_bytecode());
                        result.append(&mut value.get_bytecode());
                        result.push(Bytecode::SetTable { keep_table: false });
                    }
                    _ => panic!("Invalid assignment")
                }

                result
            }
            Tree::Block { statements, stack_size } => {
                return statements.get_bytecode();
            }
        }
    }
}