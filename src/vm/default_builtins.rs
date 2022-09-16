use crate::vm::{VM, ExternalFunction, Value};
use crate::vm::Value::Nil;

pub type BuiltinList = [(&'static str, ExternalFunction)];

pub const DEFAULT_BUILTINS: &BuiltinList = &[
    ("print", |vm: &mut VM, parameters: Vec<Value>| {
        let mut result = String::new();
        for param in parameters.iter().enumerate() {
            result.push_str(&vm.string_storage.value_to_string(param.1));
            if param.0 < parameters.len() - 1 {
                result.push(' ');
            }
        }
        writeln!(vm.output, "{}", result);
        return Ok(Nil);
    })
];