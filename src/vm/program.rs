use crate::vm::Bytecode;

pub struct Function {
    pub code: Vec<Bytecode>,
    pub stack_size: usize,
}

pub struct Program {
    pub functions: Vec<Function>,
    pub main_function: Function,
    pub string_literals: Vec<String>,
}