#[derive(Debug)]
pub enum Bytecode {
    /// Pushes a number onto the stack.
    PushNumber(f64),

    /// Pushes a string literal onto the stack.
    PushStringLiteral(usize),

    /// Pushes a boolean onto the stack.
    PushBool(bool),

    /// Pushes a nil value onto the stack.
    PushNil,

    /// Pushes the builtin onto the stack.
    PushBuiltin(usize),

    /// Pushes the local variable onto the stack.
    PushLocal(usize),

    /// Pushes a function from the program onto the stack.
    PushFunction(usize),

    /// Pops the last two numbers on the stack, and pushes their sum.
    AddNumbers,

    /// Pops the last two numbers on the stack, and pushes their subtraction (first pushed number
    /// minus the second pushed number).
    SubNumbers,

    /// Pops the last two numbers on the stack, and pushes their multiplication.
    MulNumbers,

    /// Pops the last two numbers on the stack, and pushes their division (first pushed number
    /// divided by the second pushed number).
    DivNumbers,

    /// Compares whether the top two values on the stack are equal, and pushes the according
    /// boolean.
    CompareEqual,

    /// Compares whether the pre-last value on the stack is greater than the last value, and pushes
    /// the according boolean.
    CompareGreater,

    /// Compares whether the pre-last value on the stack is less than the last value, and pushes
    /// the according boolean.
    CompareLess,

    /// Compares whether the pre-last value on the stack is greater than or equal to the last value,
    /// and pushes the according boolean.
    CompareGEqual,

    /// Compares whether the pre-last value on the stack is less than or equal to the last value,
    /// and pushes the according boolean.
    CompareLEqual,

    /// Performs the boolean && operation on the last two bools on the stack.
    BooleanAnd,

    /// Performs the boolean || operation on the last two bools on the stack.
    BooleanOr,

    /// Replaces number at the top of the stack with its negation.
    Negate,

    /// Replaces value at the top of the stack with its boolean inverse.
    Not,

    /// Pops two values off the stack, concats them into a string and pushes that.
    /// (first value pushed .. second value)
    Concat,

    /// Jumps x instructions forward. (can be negative too)
    Jump(isize),

    /// Pops a value off the stack, and jumps x instructions forward only if the value is `true`.
    JumpIfTrue(isize),

    /// Pops a value off the stack, and jumps x instructions forward only if the value is `false`.
    JumpIfFalse(isize),

    /// Calls the last value on the stack, with `num_args` previous values on the stack as
    /// parameters (in the order they were added).
    Call { num_args: usize },

    /// Stops the function and, if `true`, sets the return register to the last value on the stack.
    Return(bool),

    /// Pushes the return register to the stack.
    PushReturn,

    /// Sets the specificed local to the last value on the stack.
    SetLocal(usize),

    /// Creates a new table and pushes it onto the stack.
    CreateTable,

    /// Pops index value, then table value, and pushes what the table contains at that index.
    GetTable,

    /// Pops desired value, then index value, then table value (if specified), and sets the table's
    /// value at that index.
    SetTable { 
        /// If `true`, the table _won't_ be popped off the stack after finishing the operation.
        keep_table: bool
    },
}