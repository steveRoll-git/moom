use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::string::String;

pub use bytecode::Bytecode;
pub use program::Function;
pub use runtime_error::RuntimeError as RuntimeError;

pub use crate::vm::program::Program;
use crate::vm::Value::{Boolean, Nil};

mod bytecode;
mod program;
mod runtime_error;
pub mod default_builtins;

pub type ExternalFunction = fn(&mut VM, Vec<Value>) -> Result<Value, String>;

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    String(usize),
    Function { prototype_index: usize },
    ExternalFunction(usize),
}

impl Value {
    pub fn get_type_name(&self) -> &str {
        match self {
            Value::Nil => "nil",
            Value::Number(_) => "number",
            Value::Boolean(_) => "boolean",
            Value::String(_) => "string",
            Value::Function { .. } => "function",
            Value::ExternalFunction(_) => "external"
        }
    }

    pub fn is_truthy(&self) -> bool {
        !(*self == Nil || *self == Boolean(false))
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Nil => write!(f, "nil"),
            Value::Number(n) => write!(f, "{}", n),
            Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "<string {}>", s),
            Value::Function { prototype_index } => write!(f, "<function {}>", prototype_index),
            Value::ExternalFunction(index) => write!(f, "<builtin {}>", index)
        }
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Value::Number(v)
    }
}

impl TryFrom<Value> for f64 {
    type Error = ();

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(n) => Ok(n),
            _ => Err(())
        }
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Value::Boolean(v)
    }
}
//impl From<String> for Value {
//    fn from(v: String) -> Self {
//        Value::String(v)
//    }
//}

/// Stores all information about a running function: its instruction pointer, value stack, etc.
struct StackFrame {
    /// Index of the running function prototype.
    /// 0 - the main function, any other number is an index to the functions array, minus one
    function: usize,

    /// Stack of temporary values used for calculating expressions.
    value_stack: Vec<Value>,

    /// Local values.
    locals: Vec<Value>,

    /// Address of the bytecode instruction that is currently being executed.
    instruction_pointer: usize,
}

impl StackFrame {
    pub fn push_value(&mut self, value: Value) {
        self.value_stack.push(value);
    }

    pub fn pop_value(&mut self) -> Result<Value, RuntimeError> {
        if let Some(value) = self.value_stack.pop() {
            Ok(value)
        } else {
            Err(RuntimeError::OperationOnEmptyStack)
        }
    }

    fn binary_math_op(&mut self, func: fn(f64, f64) -> f64) -> Result<(), RuntimeError> {
        let b = self.pop_value()?;
        let a = self.pop_value()?;

        if let (Value::Number(a), Value::Number(b)) = (a, b) {
            let result = func(a, b);
            self.push_value(Value::Number(result));
            return Ok(());
        } else {
            return Err(RuntimeError::ArithmeticOnNonNumbers(a, b));
        }
    }

    fn binary_arithmetic_compare_op(&mut self, func: fn(f64, f64) -> bool) -> Result<(), RuntimeError> {
        let b = self.pop_value()?;
        let a = self.pop_value()?;

        if let (Value::Number(a), Value::Number(b)) = (a, b) {
            let result = func(a, b);
            self.push_value(Value::Boolean(result));
            return Ok(());
        }

        return Err(RuntimeError::ComparingMismatchedTypes(a, b));
    }

    fn binary_bool_op(&mut self, func: fn(bool, bool) -> bool) -> Result<(), RuntimeError> {
        let b = self.pop_value()?;
        let a = self.pop_value()?;

        let a = a.is_truthy();
        let b = b.is_truthy();

        self.push_value(Value::Boolean(func(a, b)));

        Ok(())
    }

    fn compare_equal(&mut self) -> Result<(), RuntimeError> {
        let b = self.pop_value()?;
        let a = self.pop_value()?;

        self.push_value(Value::Boolean(a == b));

        Ok(())
    }
}

pub struct VM {
    stack_frames: Vec<StackFrame>,
    program: Program,
    result: Value,
    string_storage: HashMap<usize, String>,
    external_functions: Vec<ExternalFunction>,
    string_last_id: usize,
}

impl VM {
    pub fn new(program: Program, builtins: Option<Vec<ExternalFunction>>) -> VM {
        VM {
            stack_frames: vec![],
            program,
            result: Value::Nil,
            string_storage: Default::default(),
            string_last_id: 0,
            external_functions: match builtins {
                None => default_builtins::DEFAULT_BUILTINS.iter().map(|func| func.1).collect(),
                Some(builtins) => builtins
            },
        }
    }

    fn last_frame(&mut self) -> &mut StackFrame {
        self.stack_frames.last_mut().expect("Tried to run without any stack frames")
    }

    fn run_instruction(&mut self) -> Result<(), RuntimeError> {
        let last_frame = self
            .stack_frames
            .last_mut()
            .expect("tried to run without any stack frames");

        let function: &Function = if last_frame.function == 0 {
            &self.program.main_function
        } else {
            &self.program.functions[last_frame.function - 1]
        };
        let instruction: &Bytecode = function
            .get(last_frame.instruction_pointer)
            .expect("instruction pointer out of range");

        let mut next_instruction: usize = last_frame.instruction_pointer + 1;

        match instruction {
            Bytecode::PushNumber(n) => {
                last_frame.push_value(Value::Number(*n));
            }
            Bytecode::PushString(s) => {
                self.string_storage.insert(self.string_last_id, s.clone());
                last_frame.push_value(Value::String(self.string_last_id));
                self.string_last_id += 1;
            }
            Bytecode::PushBool(b) => {
                last_frame.push_value(Value::Boolean(*b));
            }
            Bytecode::PushBuiltin(i) => {
                last_frame.push_value(Value::ExternalFunction(*i));
            }
            Bytecode::PushLocal(i) => {
                last_frame.push_value(last_frame.locals[*i]);
            }
            Bytecode::AddNumbers => {
                last_frame.binary_math_op(|a: f64, b: f64| a + b)?;
            }
            Bytecode::SubNumbers => {
                last_frame.binary_math_op(|a: f64, b: f64| a - b)?;
            }
            Bytecode::MulNumbers => {
                last_frame.binary_math_op(|a: f64, b: f64| a * b)?;
            }
            Bytecode::DivNumbers => {
                last_frame.binary_math_op(|a: f64, b: f64| a / b)?;
            }
            Bytecode::CompareEqual => {
                last_frame.compare_equal()?;
            }
            Bytecode::CompareGreater => {
                last_frame.binary_arithmetic_compare_op(|a: f64, b: f64| a > b)?;
            }
            Bytecode::CompareLess => {
                last_frame.binary_arithmetic_compare_op(|a: f64, b: f64| a < b)?;
            }
            Bytecode::CompareGEqual => {
                last_frame.binary_arithmetic_compare_op(|a: f64, b: f64| a >= b)?;
            }
            Bytecode::CompareLEqual => {
                last_frame.binary_arithmetic_compare_op(|a: f64, b: f64| a <= b)?;
            }
            Bytecode::BooleanAnd => {
                last_frame.binary_bool_op(|a: bool, b: bool| a && b)?;
            }
            Bytecode::BooleanOr => {
                last_frame.binary_bool_op(|a: bool, b: bool| a || b)?;
            }
            Bytecode::Negate => {
                let val = last_frame.pop_value()?;
                let a: Result<f64, ()> = val.try_into();
                match a {
                    Ok(n) => {
                        last_frame.push_value(Value::Number(-n));
                    }
                    Err(..) => {
                        return Err(RuntimeError::NegateNonNumber(val));
                    }
                }
            }
            Bytecode::Not => {
                let a = last_frame.pop_value()?;
                last_frame.push_value(Value::Boolean(!a.is_truthy()));
            }
            Bytecode::Jump(amount) => {
                next_instruction = (last_frame.instruction_pointer as isize + amount) as usize;
            }
            Bytecode::JumpIfTrue(amount) => {
                let cond = last_frame.pop_value()?;
                if cond.is_truthy() {
                    next_instruction = (last_frame.instruction_pointer as isize + amount) as usize;
                }
            }
            Bytecode::JumpIfFalse(amount) => {
                let cond = last_frame.pop_value()?;
                if !cond.is_truthy() {
                    next_instruction = (last_frame.instruction_pointer as isize + amount) as usize;
                }
            }
            Bytecode::Call { num_args } => {
                let the_function = last_frame.pop_value()?;
                match the_function {
                    Value::Nil | Value::Number(_) | Value::Boolean(_) | Value::String(_) => {
                        return Err(RuntimeError::CallNonFunction(the_function));
                    }
                    _ => {}
                }
                let mut parameters = vec![Value::Nil; *num_args];
                for i in 0..*num_args {
                    parameters[num_args - i - 1] = last_frame.pop_value()?;
                }
                match the_function {
                    Value::Function { prototype_index } => {
                        todo!()
                    }
                    Value::ExternalFunction(index) => {
                        let the_function = self.external_functions[index];
                        let result = the_function(self, parameters);
                        match result {
                            Ok(value) => {
                                self.result = value;
                            },
                            Err(message) => {
                                return Err(RuntimeError::Custom(message));
                            },
                        }
                    }
                    _ => {}
                }
            }
            Bytecode::Return(return_value) => {
                self.result = if *return_value {
                    last_frame.value_stack.pop().unwrap_or(Value::Nil)
                } else {
                    Value::Nil
                };
                self.stack_frames.pop();
                return Ok(());
            }
            Bytecode::PushReturn => {
                last_frame.push_value(self.result);
            }
        }

        self.last_frame().instruction_pointer = next_instruction;

        Ok(())
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        self.stack_frames.push(StackFrame {
            function: 0,
            value_stack: vec![],
            locals: vec![],
            instruction_pointer: 0,
        });
        while !self.stack_frames.is_empty() {
            self.run_instruction()?;
        }
        return Ok(());
    }

    pub fn get_result(&self) -> Value {
        self.result.clone()
    }
}

#[cfg(test)]
mod vm_tests {
    use std::option::Option::None;

    use crate::lang::Parser;

    use super::*;

    fn assert_expr(code: &'static str, expected_result: Value) {
        let mut parser = Parser::new(Box::new(code.chars()), "code".to_string(), None);
        let program = parser.parse_expression_program();
        match program {
            Ok(program) => {
                let mut vm = VM::new(program, None);
                match vm.run() {
                    Ok(_) => {

                    }
                    Err(error) => {
                        panic!("Runtime Error:\n{}", error);
                    }
                }
                assert_eq!(vm.result, expected_result);
            }
            Err(error) => {
                println!("Syntax Error:\n{}", error);
                panic!();
            }
        }
    }

    fn assert_program(code: &'static str, expected_result: Value) {
        let mut parser = Parser::new(Box::new(code.chars()), "code".to_string(), None);
        let program = parser.parse_file();
        match program {
            Ok(program) => {
                println!("{:?}", program.main_function);
                let mut vm: VM = VM::new(program, None);
                match vm.run() {
                    Ok(_) => {}
                    Err(error) => {
                        panic!("Runtime Error:\n{}", error)
                    }
                }
                let result = vm.get_result();
                assert_eq!(result, expected_result);
            }
            Err(e) => {
                println!("Error: {}", e);
                panic!();
            }
        }
    }

    #[test]
    fn test_expr() {
        assert_expr("5 * 4 - (9) / 3 * 10 + 78", Value::Number(68.0));
        assert_expr("-(8 / 7) * 6 * 3 / (78 - -4) + 3", Value::Number(2.749128919860627));
    }

    #[test]
    fn test_bool() {
        assert_expr("true", Value::Boolean(true));
        assert_expr("false", Value::Boolean(false));
        assert_expr("!true", Value::Boolean(false));
        assert_expr("6 > 7 && 2 <= 0", Value::Boolean(false));
        assert_expr("false || 4 >= 3", Value::Boolean(true));
        assert_expr("!(4 > -999) || 0 != 0", Value::Boolean(false));
    }
}