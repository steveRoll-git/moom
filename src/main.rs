use lang::{Parser, linker::link};

use crate::vm::VM;

mod lang;
mod vm;
mod get_hash;

fn main() {
    //TODO this code is temporary, and is mainly here to get rid of deadcode warnings
    let code = "
    func main() {
        print(\"moom works!\");
    }";
    let mut parser = Parser::new(Box::new(code.chars()), "code".to_string(), None);
    let file = parser.parse_file();
    let mut syntax_error = None;
    match file {
        Ok(file) => {
            let program = link(vec![file]);
            match program {
                Ok(program) => {
                    let mut vm: VM = VM::new(program, None);
                    match vm.run() {
                        Ok(_) => {}
                        Err(error) => {
                            panic!("Runtime Error:\n{}", error)
                        }
                    }
                },
                Err(e) => {
                    syntax_error = Some(e);
                },
            }
        }
        Err(e) => {
            syntax_error = Some(e);
        }
    }

    if let Some(e) = syntax_error {
        panic!("Error: {}", e);
    }
}
