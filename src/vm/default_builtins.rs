use crate::vm::{VM, ExternalFunction, Value};
use crate::vm::Value::Nil;

pub type BuiltinList = [(&'static str, ExternalFunction)];

pub const DEFAULT_BUILTINS: &BuiltinList = &[
    ("print", |vm: &mut VM, parameters: Vec<Value>| {
        let mut result = String::new();
        for param in parameters.iter().enumerate() {
            match param.1 {
                Value::String(index) => {
                    result.push_str(vm.string_storage.get(index).expect("string doesn't exist"));
                }
                _ => {
                    result.push_str(&*param.1.to_string());
                }
            };
            if param.0 < parameters.len() - 1 {
                result.push(' ');
            }
        }
        writeln!(vm.output, "{}", result);
        return Ok(Nil);
    })
];