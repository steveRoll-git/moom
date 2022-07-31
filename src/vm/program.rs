use crate::vm::Bytecode;

pub type Function = Vec<Bytecode>;

pub struct Program {
    pub functions: Vec<Function>,
    pub main_function: Function
}