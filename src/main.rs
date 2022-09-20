use lang::Parser;

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
    let program = parser.parse_file();
    match program {
        Ok(program) => {
            let mut vm: VM = VM::new(program, None);
            match vm.run() {
                Ok(_) => {}
                Err(error) => {
                    panic!("Runtime Error:\n{}", error)
                }
            }
        }
        Err(e) => {
            println!("Error: {}", e);
            panic!();
        }
    }
}
