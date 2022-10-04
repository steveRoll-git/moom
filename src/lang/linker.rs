use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use crate::vm::{Function, Program, Bytecode};

use super::{
    syntax_tree::{
        Binding, FileTree, FunctionDef, LateReference, StringLiteral, TraverseTree, Tree,
    },
    Position, SyntaxError, SyntaxErrorKind, ToBytecode,
};

pub fn link(mut files: Vec<FileTree>) -> Result<Program, SyntaxError> {
    let mut bindings: HashMap<String, Binding> = HashMap::new();
    let mut function_defs: Vec<FunctionDef> = vec![];
    let mut main_function: Option<usize> = None;
    let mut string_literals = vec![];

    while !files.is_empty() {
        let mut file = files.pop().unwrap();
        while !file.function_defs.is_empty() {
            let func = file.function_defs.pop().unwrap();
            if &func.name == "main" {
                main_function = Some(function_defs.len());
            }
            if bindings.contains_key(&func.name) {
                return Err(SyntaxError {
                    error: SyntaxErrorKind::DuplicateFunctionDefinition(func.name),
                    position: func.position_defined,
                });
            }
            bindings.insert(func.name.clone(), Binding::Function(function_defs.len()));
            function_defs.push(func);
        }
    }

    if main_function.is_none() {
        return Err(SyntaxError {
            error: SyntaxErrorKind::MissingMainFunction,
            position: Position {
                line: 0,
                column: 0,
                source_name: "".to_string(),
            },
        });
    }

    let main_function = main_function.unwrap();

    for func in &function_defs {
        //TODO make a more sophisticated process that can show multiple errors
        let mut error: Option<SyntaxError> = None;
        func.body.traverse(&mut |tree| {
            if let Tree::BindingValue(binding) = tree {
                if let Binding::LateReference(r) = binding {
                    let mut r = r.borrow_mut();
                    if let LateReference::Unresolved(ident) = r.deref() {
                        if let Some(binding) = bindings.get(&ident.name) {
                            *r.deref_mut() = LateReference::Resolved(Box::new(binding.clone()));
                        } else {
                            error = Some(SyntaxError {
                                error: SyntaxErrorKind::UnresolvedName(ident.name.clone()),
                                position: ident.position.clone(),
                            })
                        }
                    }
                }
            } else if let Tree::StringLiteral(lit) = tree {
                let mut r = lit.borrow_mut();
                if let StringLiteral::NotIndexed(s) = r.deref() {
                    let index = if let Some(index) = string_literals.iter().position(|o| o == s) {
                        index
                    } else {
                        string_literals.push(s.clone());
                        string_literals.len() - 1
                    };
                    *r.deref_mut() = StringLiteral::Indexed(index);
                }
            }
        });
        if let Some(error) = error {
            return Err(error);
        }
    }

    let mut functions: Vec<Function> = vec![];

    for func in &function_defs {
        let mut code = func.body.get_bytecode();
        if !matches!(code.last(), Some(Bytecode::Return(_))) {
            code.push(Bytecode::Return(false));
        }
        functions.push(Function {
            code,
            num_params: func.params.len(),
            stack_size: func.stack_size,
        });
    }

    Ok(Program {
        functions,
        main_function,
        string_literals,
    })
}
