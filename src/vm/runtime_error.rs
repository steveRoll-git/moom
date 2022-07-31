use std::fmt::{Display, Formatter};
use crate::vm::Value;

pub enum RuntimeError {
    /// Tried to do an operation that requires a value(s) on the stack, but the stack was empty
    OperationOnEmptyStack,
    /// Tried to do arithmetic operations (like adding) on values that aren't numbers
    ArithmeticOnNonNumbers(Value, Value),
    /// Tried to negate a value that isn't a number
    NegateNonNumber(Value),
    /// Tried using a numerical comparison (like <=) on values that aren't numbers
    ComparingMismatchedTypes(Value, Value),
    /// Tried calling a value that can't be called
    CallNonFunction(Value),
    /// Custom error, used by builtin functions.
    Custom(String),
}
impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::OperationOnEmptyStack => {
                write!(f, "Stack is empty")
            }
            RuntimeError::ArithmeticOnNonNumbers(a, b) => {
                write!(f, "Can't perform arithmetic on {} and {} values", a.get_type_name(), b.get_type_name())
            }
            RuntimeError::NegateNonNumber(a) => {
                write!(f, "Can't negate a {} value", a.get_type_name())
            }
            RuntimeError::ComparingMismatchedTypes(a, b) => {
                write!(f, "Can't compare {} and {} values", a.get_type_name(), b.get_type_name())
            }
            RuntimeError::CallNonFunction(a) => {
                write!(f, "Can't call a {} value", a.get_type_name())
            }
            RuntimeError::Custom(message) => {
                write!(f, "{}", message)
            },
        }
    }
}