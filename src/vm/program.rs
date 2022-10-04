use crate::vm::Bytecode;

#[derive(Debug)]
pub struct Function {
    pub code: Vec<Bytecode>,
    pub num_params: usize,
    pub stack_size: usize,
}

pub struct Program {
    pub functions: Vec<Function>,
    pub main_function: usize,
    pub string_literals: Vec<String>,
}

pub enum FunctionRef {
    MainFunction,
    Function(usize)
}