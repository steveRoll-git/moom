use std::cell::Cell;
use std::cmp::max;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::string::String;

use slotmap::{SlotMap, Key, new_key_type};
use crate::get_hash::GetHash;

pub use bytecode::Bytecode;
pub(crate) use program::Function;
pub use runtime_error::RuntimeError;

pub use crate::vm::program::Program;
use crate::vm::Value::{Boolean, Nil};

use self::default_builtins::{BuiltinList, DEFAULT_BUILTINS};
use self::program::FunctionRef;

mod bytecode;
pub mod default_builtins;
mod program;
mod runtime_error;

pub type ExternalFunction = fn(&mut VM, Vec<Value>) -> Result<Value, String>;

type Table = HashMap<u64, Value>;

const GC_GROWTH_FACTOR: f64 = 1.4;
const GC_MIN_HEAP_SIZE: usize = 4 * 1024;

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    String(StringKey),
    Table(TableKey),
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
            Value::Table(_) => "table",
            Value::Function { .. } => "function",
            Value::ExternalFunction(_) => "external",
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
            Value::String(s) => write!(f, "<string {:#x}>", s.data().as_ffi()),
            Value::Table(t) => write!(f, "<table {:#x}>", t.data().as_ffi()),
            Value::Function { prototype_index } => write!(f, "<function {}>", prototype_index),
            Value::ExternalFunction(index) => write!(f, "<builtin {}>", index),
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
            _ => Err(()),
        }
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Value::Boolean(v)
    }
}

trait Markable {
    fn mark(&self, vm: &VM, new_mark: u8) {}
}

impl Markable for String {}

impl Markable for Table {
    fn mark(&self, vm: &VM, new_mark: u8) {
        for (_, v) in self {
            v.mark(vm, new_mark);
        }
    }
}

impl Markable for Value {
    fn mark(&self, vm: &VM, new_mark: u8) {
        match self {
            Value::String(index) => {
                vm.string_storage.mark_at(vm, new_mark, *index);
            },
            Value::Table(index) => {
                vm.table_storage.mark_at(vm, new_mark, *index);
            }
            _ => {}
        }
    }
}

struct GCObject<T: Markable> {
    mark: Cell<u8>,
    value: T
}

impl<T: Markable> GCObject<T> {
    fn mark(&self, vm: &VM, new_mark: u8) {
        if self.mark.get() != new_mark {
            self.mark.set(new_mark);
            self.value.mark(vm, new_mark);
        }
    }
}

const DELETED_MESSAGE: &str = "Object was deleted!";

#[derive(Default)]
struct GCObjectStorage<K: Key, V: Markable> {
    storage: SlotMap<K, GCObject<V>>,
}

impl<K: Key, V: Markable> GCObjectStorage<K, V> {
    fn get_object(&self, index: K) -> &GCObject<V> {
        self.storage.get(index).expect(DELETED_MESSAGE)
    }

    fn get_object_mut(&mut self, index: K) -> &mut GCObject<V> {
        self.storage.get_mut(index).expect(DELETED_MESSAGE)
    }

    fn insert(&mut self, value: V, gc_state: &mut GCState) -> K {
        let object = GCObject { mark: Cell::new(gc_state.mark), value };
        gc_state.live_bytes += std::mem::size_of::<V>();
        let the_index = self.storage.insert(object);
        the_index
    }

    fn mark_at(&self, vm: &VM, new_mark: u8, index: K) {
        let object = self.get_object(index);
        object.mark(vm, new_mark);
    }

    fn sweep(&mut self, gc_state: &mut GCState) {
        let mut count = self.storage.len();
        self.storage.retain(|_k, v| {
            let cond = v.mark.get() == gc_state.mark;
            if !cond {
                gc_state.live_bytes -= std::mem::size_of::<V>();
                count -= 1;
            }
            cond
        });
    }
}

new_key_type! { pub struct StringKey; }
new_key_type! { pub struct TableKey; }

impl GCObjectStorage<StringKey, String> {
    fn value_to_string(&self, value: &Value) -> String {
        match value {
            Value::String(index) => {
                self.get_object(*index).value.clone()
            },
            _ => value.to_string(),
        }
    }
}

/// Stores all information about a running function: its instruction pointer, value stack, etc.
struct StackFrame {
    /// Index of the running function prototype.
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

    fn binary_arithmetic_compare_op(
        &mut self,
        func: fn(f64, f64) -> bool,
    ) -> Result<(), RuntimeError> {
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

struct GCState {
    mark: u8,
    live_bytes: usize,
    gc_threshold: usize,
}

pub struct VM {
    stack_frames: Vec<StackFrame>,
    program: Program,
    result: Value,
    string_storage: GCObjectStorage<StringKey, String>,
    table_storage: GCObjectStorage<TableKey, Table>,
    gc_state: GCState,
    external_functions: Vec<ExternalFunction>,

    pub output: Box<dyn Write>,
}

impl VM {
    pub fn new(program: Program, builtins: Option<&BuiltinList>) -> VM {
        VM {
            stack_frames: vec![],
            program,
            result: Value::Nil,
            string_storage: Default::default(),
            table_storage: Default::default(),
            gc_state: GCState { mark: 0, live_bytes: 0, gc_threshold: 4 * 1024 },
            external_functions: {
                let the_builtins = builtins.unwrap_or(DEFAULT_BUILTINS);
                the_builtins.iter().map(|func| func.1).collect()
            },
            output: Box::new(std::io::stdout()),
        }
    }

    fn last_frame(&mut self) -> &mut StackFrame {
        self.stack_frames
            .last_mut()
            .expect("Tried to run without any stack frames")
    }

    fn run_instruction(&mut self) -> Result<(), RuntimeError> {
        let last_frame = self
            .stack_frames
            .last_mut()
            .expect("tried to run without any stack frames");

        let function: &Function = &self.program.functions[last_frame.function];
        let instruction: &Bytecode = function.code
            .get(last_frame.instruction_pointer)
            .expect("instruction pointer out of range");

        let mut next_instruction: usize = last_frame.instruction_pointer + 1;

        const EMPTY_STACK_MESSAGE: &str = "Stack is empty";

        match instruction {
            Bytecode::PushNumber(n) => {
                last_frame.push_value(Value::Number(*n));
            }
            Bytecode::PushStringLiteral(index) => {
                let index = self.string_storage.insert(self.program.string_literals[*index].clone(), &mut self.gc_state);
                last_frame.push_value(Value::String(index));
            }
            Bytecode::PushBool(b) => {
                last_frame.push_value(Value::Boolean(*b));
            }
            Bytecode::PushNil => {
                last_frame.push_value(Value::Nil);
            }
            Bytecode::PushBuiltin(i) => {
                last_frame.push_value(Value::ExternalFunction(*i));
            }
            Bytecode::PushLocal(i) => {
                last_frame.push_value(last_frame.locals[*i]);
            }
            Bytecode::PushFunction(i) => {
                last_frame.push_value(Value::Function { prototype_index: *i });
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
            Bytecode::Concat => {
                let b = last_frame.pop_value()?;
                let a = last_frame.pop_value()?;

                let mut result = self.string_storage.value_to_string(&a);
                result.push_str(&self.string_storage.value_to_string(&b));

                let index = self.string_storage.insert(result, &mut self.gc_state);
                last_frame.push_value(Value::String(index));
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
                        last_frame.instruction_pointer = next_instruction;
                        let the_function = &self.program.functions[prototype_index];
                        let mut locals = parameters;
                        for _ in 0..the_function.num_params - locals.len() {
                            locals.push(Value::Nil);
                        }
                        self.stack_frames.push(StackFrame {
                            function: prototype_index,
                            value_stack: vec![],
                            locals,
                            instruction_pointer: 0,
                        });
                        return Ok(());
                    }
                    Value::ExternalFunction(index) => {
                        let the_function = self.external_functions[index];
                        let result = the_function(self, parameters);
                        match result {
                            Ok(value) => {
                                self.result = value;
                            }
                            Err(message) => {
                                return Err(RuntimeError::Custom(message));
                            }
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
            Bytecode::SetLocal(index) => {
                last_frame.locals[*index] = last_frame.value_stack.pop().expect(EMPTY_STACK_MESSAGE);
            },
            Bytecode::CreateTable => {
                let index = self.table_storage.insert(HashMap::new(), &mut self.gc_state);
                last_frame.value_stack.push(Value::Table(index));
            },
            Bytecode::GetTable => {
                let index = last_frame.value_stack.pop().expect(EMPTY_STACK_MESSAGE);
                let table = last_frame.value_stack.pop().expect(EMPTY_STACK_MESSAGE);

                if let Value::Table(table_index) = table {
                    if let Value::String(string_index) = index {
                        let the_table = &self.table_storage.get_object(table_index).value;
                        let the_index = &self.string_storage.get_object(string_index).value;

                        last_frame.value_stack.push(*the_table.get(&the_index.get_hash()).unwrap_or(&Value::Nil));
                    } else {
                        return Err(RuntimeError::IndexWithNonString(index));
                    }
                } else {
                    return Err(RuntimeError::IndexNonTable(table));
                }
            },
            Bytecode::SetTable { keep_table } => {
                let value = last_frame.value_stack.pop().expect(EMPTY_STACK_MESSAGE);
                let index = last_frame.value_stack.pop().expect(EMPTY_STACK_MESSAGE);
                let table = last_frame.value_stack.last().expect(EMPTY_STACK_MESSAGE);
                
                if let Value::Table(table_index) = table {
                    if let Value::String(string_index) = index {
                        let the_table = &mut self.table_storage.get_object_mut(*table_index).value;
                        let the_index = &self.string_storage.get_object(string_index).value;
                        the_table.insert(the_index.get_hash(), value);
                    } else {
                        return Err(RuntimeError::IndexWithNonString(index));
                    }
                } else {
                    return Err(RuntimeError::IndexNonTable(*table));
                }

                if !keep_table {
                    last_frame.value_stack.pop();
                }
            },
        }

        self.last_frame().instruction_pointer = next_instruction;

        if self.gc_state.live_bytes > self.gc_state.gc_threshold && self.last_frame().value_stack.len() == 0 {
            self.collect_garbage();
        }

        Ok(())
    }

    pub fn collect_garbage(&mut self) {
        self.gc_state.mark = 1 - self.gc_state.mark;

        for frame in &self.stack_frames {
            for val in &frame.locals {
                val.mark(self, self.gc_state.mark);
            }
            for val in &frame.value_stack {
                val.mark(self, self.gc_state.mark);
            }
        }

        self.result.mark(self, self.gc_state.mark);

        self.string_storage.sweep(&mut self.gc_state);
        self.table_storage.sweep(&mut self.gc_state);

        self.gc_state.gc_threshold = max((self.gc_state.live_bytes as f64 * GC_GROWTH_FACTOR) as usize, GC_MIN_HEAP_SIZE);
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        self.stack_frames.push(StackFrame {
            function: 0,
            value_stack: vec![],
            locals: vec![Value::Nil; self.program.functions[self.program.main_function].stack_size],
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
