use std::{
    option::Option::None,
    sync::{Arc, Mutex},
    io::Write,
};

use moom::lang::{*, linker::link};
use moom::vm::*;

fn assert_expr(code: &'static str, expected_result: Value) {
    let mut parser = Parser::new(Box::new(code.chars()), "code".to_string(), None);
    let program = parser.parse_expression_program();
    match program {
        Ok(program) => {
            let mut vm = VM::new(program, None);
            match vm.run() {
                Ok(_) => {}
                Err(error) => {
                    panic!("Runtime Error:\n{}", error);
                }
            }
            assert_eq!(vm.get_result(), expected_result);
        }
        Err(error) => {
            println!("Syntax Error:\n{}", error);
            panic!();
        }
    }
}

fn assert_program(code: &'static str, expected_output: &str) {
    #[derive(Clone)]
    struct OutputCapturer(Arc<Mutex<Vec<u8>>>);
    impl Write for OutputCapturer {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.0.lock().unwrap().extend(buf);
            Ok(buf.len())
        }

        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    let mut parser = Parser::new(Box::new(code.chars()), "code".to_string(), None);
    let file = parser.parse_file();
    match file {
        Ok(file) => {
            let program = link(vec![file]);

            match program {
                Ok(program) => {
                    for (_, func) in program.functions.iter().enumerate() {
                        println!("{:?}", func);
                    }

                    let mut vm: VM = VM::new(program, None);
                    let output = OutputCapturer(Default::default());
                    vm.output = Box::new(output.clone());
                    match vm.run() {
                        Ok(_) => {}
                        Err(error) => {
                            panic!("Runtime Error:\n{}", error)
                        }
                    }
                    assert_eq!(
                        String::from_utf8_lossy(output.0.lock().unwrap().as_slice()),
                        expected_output
                    );
                }
                Err(error) => {
                    panic!("Error: {}", error);
                }
            }
        }
        Err(e) => {
            panic!("Error: {}", e);
        }
    }
}

#[test]
fn test_expr() {
    assert_expr("5 * 4 - (9) / 3 * 10 + 78", Value::Number(68.0));
    assert_expr(
        "-(8 / 7) * 6 * 3 / (78 - -4) + 3",
        Value::Number(2.749128919860627),
    );
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

#[test]
fn test_print_literal() {
    assert_program(
        r#"
        func main() {
            print("hello wow")
            print("second line!")
        }"#,
        "hello wow\nsecond line!\n",
    )
}

#[test]
fn test_local() {
    assert_program(
        r#"
            func main() {
                var first = 124
                print(first)
                var someString = "wow very cool"
                var second = first * 1.5
                print(second, first)
                first = first + 1
                print(first, second)
                print(someString)
            }"#,
        "124\n186 124\n125 186\nwow very cool\n",
    )
}

#[test]
fn test_if() {
    assert_program(
        r#"
        func main() {
            var what = 53.3

            if what > 3 {
                print("okay good")
            } else {
                print("not supposed to happen")
            }

            if what == -1 {
                print("not good")
            } else {
                print("go on")
            }

            if what == 1121 {
                print("ASDFASDF")
            }

            if what <= 100 {
                print("yes")
            }

            if 2 == 5 {
                print("nope")
            } elseif what == what + 1 {
                print("aaaah")
            } elseif what == 53.3 {
                print("perfect")
            } else {
                print("crap")
            }
        }
        "#,
        "okay good\ngo on\nyes\nperfect\n",
    )
}

#[test]
fn test_while() {
    assert_program(
        r#"
        func main() {
            var count = 0

            print("start")

            while count < 4 {
                print("count is", count)
                count = count + 1
            }

            while false {
                print("shouldn't happen")
            }

            print("done")
        }
        "#,
        "start\ncount is 0\ncount is 1\ncount is 2\ncount is 3\ndone\n",
    )
}

#[test]
fn test_table() {
    assert_program(
        r#"
        func main() {
            var cool = {thing = 123, "woah string index" = "very value", x3 = -2, inside = {b = 656}}
            print(cool["woah string index"], cool.thing)
            cool.thing = "2525"
            print(cool["thing"], cool.inside.b)
            cool["fdfdf"] = true
            cool.x3 = cool.x3 * 2
            print(cool.fdfdf, cool.x3, cool.what)
        }
        "#,
        "very value 123\n2525 656\ntrue -4 nil\n",
    )
}

#[test]
fn test_func() {
    assert_program(
        r#"
        func square(x){
            return x * x
        }
        func factorial(x) {
            if x == 1 {
                return 1
            } else {
                return x * factorial(x - 1)
            }
        }
        func sayHello(name, time) {
            print("Hello " .. name .. ", have a nice " .. time .. "!")
        }
        func a(x) {
            if x == 0 {
                return "a0"
            }
            return "a" .. x .. b(x - 1)
        }
        func b(x) {
            if x == 0 {
                return "b0"
            }
            return "b" .. x .. a(x - 1)
        }
        func main() {
            print(square(5.5) - 1)
            sayHello("people", "evening")
            print(factorial(6))
            print(a(4))
        }
        "#,
        "29.25\nHello people, have a nice evening!\n720\na4b3a2b1a0\n",
    )
}

#[test]
fn test_string() {
    assert_program(
        r#"
        func main() {
            print(1 .. 2 .. "af" .. nil .. 3)
        }
        "#,
        "12afnil3\n",
    )
}
