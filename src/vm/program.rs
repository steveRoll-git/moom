use crate::vm::Bytecode;

#[derive(Debug)]
pub struct Function {
    pub code: Vec<Bytecode>,
    pub num_params: usize,
    pub stack_size: usize,
}

pub struct Program {
    pub functions: Vec<Function>,
    pub main_function: Function,
    pub string_literals: Vec<String>,
}

pub enum FunctionRef {
    MainFunction,
    Function(usize)
}

impl Program {
    pub fn get_function(&self, function_ref: &FunctionRef) -> &Function {
        match function_ref {
            FunctionRef::MainFunction => &self.main_function,
            FunctionRef::Function(i) => &self.functions[*i]
        }
    }
}