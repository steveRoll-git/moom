use std::cmp::max;
use std::collections::HashMap;
use std::default;

use crate::lang::{Lexer, Position, SyntaxError, SyntaxErrorKind, ToBytecode, Token};
use crate::lang::token::{Keyword, Punctuation};
use crate::lang::tree::{BinaryOperator, Binding, Tree, UnaryOperator};
use crate::lang::tree::Tree::{BoolValue, NumberValue, StringLiteralValue};
use crate::vm::{Bytecode, Function, Program};
use crate::vm::default_builtins::{DEFAULT_BUILTINS, BuiltinList};

use super::tree::IfPart;

const INFIX_ERROR_MESSAGE: &str = "Invalid infix expression state";

struct Scope {
    pub bindings: HashMap<String, Binding>,
    pub stack_size: usize,
    pub last_local: usize,
}

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    builtins: HashMap<String, usize>,
    string_literals: Vec<String>,
    binding_scopes: Vec<Scope>,
}

impl Parser {
    pub fn new(code_iterator: Box<dyn Iterator<Item=char>>, source_name: String, builtins: Option<&BuiltinList>) -> Parser {
        Parser {
            lexer: Lexer::new(code_iterator, source_name),
            current_token: Token::EOF,
            builtins: {
                    let mut bindings = HashMap::new();
                    let the_builtins = builtins.unwrap_or(DEFAULT_BUILTINS);
                    for (i, name) in the_builtins.iter().map(|func| func.0.to_string()).enumerate() {
                        bindings.insert(name.clone(), i);
                    }
                    bindings

            },
            string_literals: vec![],
            binding_scopes: vec![],
        }
    }

    pub fn source_name(&self) -> String {
        self.lexer.source_name()
    }

    pub fn position(&self) -> Position {
        self.lexer.position()
    }

    pub fn previous_position(&self) -> Position {
        self.lexer.previous_position()
    }

    fn next_token(&mut self) -> Result<(), SyntaxError> {
        self.current_token = self.lexer.next()?;
        Ok(())
    }

    fn consume(&mut self) -> Result<Token, SyntaxError> {
        self.lexer.next()
    }

    fn unexpected_token<T>(&self, position: Position, token: Token) -> Result<T, SyntaxError> {
        Err(SyntaxError {
            error: SyntaxErrorKind::UnexpectedToken(token),
            source_name: self.source_name(),
            position,
        })
    }

    fn expect_punctuation(&mut self, expect: Punctuation) -> Result<(), SyntaxError> {
        let position = self.previous_position();
        if let Token::Punctuation(p) = self.current_token {
            if p == expect {
                self.next_token()?;
                return Ok(());
            }
        }

        Err(SyntaxError {
            error: SyntaxErrorKind::ExpectedXButGotY { expected: Token::Punctuation(expect), got: self.current_token.clone() },
            source_name: self.source_name(),
            position,
        })
    }

    fn expect_keyword(&mut self, expect: Keyword) -> Result<(), SyntaxError> {
        let position = self.position();
        if let Token::Keyword(k) = self.current_token {
            if k == expect {
                self.next_token()?;
                return Ok(());
            }
        }

        Err(SyntaxError {
            error: SyntaxErrorKind::ExpectedXButGotY { expected: Token::Keyword(expect), got: self.current_token.clone() },
            source_name: self.source_name(),
            position,
        })
    }

    fn expect_identifier(&mut self) -> Result<String, SyntaxError> {
        let position = self.position();
        if let Token::Identifier(str) = self.current_token.clone() {
            self.next_token()?;
            return Ok(str);
        }

        Err(SyntaxError {
            error: SyntaxErrorKind::ExpectedIdentifierButGotX(self.current_token.clone()),
            source_name: self.source_name(),
            position,
        })
    }

    fn get_binding(&self, name: &str) -> Option<Binding> {
        for scope in self.binding_scopes.iter().rev() {
            if let Some(binding) = scope.bindings.get(name) {
                return Some(*binding);
            }
        }
        if let Some(index) = self.builtins.get(name) {
            return Some(Binding::Builtin(*index))
        }

        None
    }

    fn get_string_literal(&mut self, literal: &String) -> Tree {
        Tree::StringLiteralValue(
            if let Some(index) = self.string_literals.iter().position(|x| *x == *literal) {
                index
            } else {
                self.string_literals.push(literal.clone());
                self.string_literals.len() - 1
            }
        )
    }

    fn parse_primary(&mut self) -> Result<Tree, SyntaxError> {
        let position = self.position();
        let token = self.current_token.clone();
        let result = match token {
            Token::EOF => { self.unexpected_token(position, token) }
            Token::Number(n) => Ok(NumberValue(n)),
            Token::String(s) => {
                Ok(self.get_string_literal(&s))
            },
            Token::Keyword(k) => {
                match k {
                    Keyword::True => Ok(BoolValue(true)),
                    Keyword::False => Ok(BoolValue(false)),
                    _ => self.unexpected_token(position, token)
                }
            }
            //TODO simplify this when `if let` guards are stabilized
            Token::Punctuation(p) if matches!(UnaryOperator::get_operator(p), Some(_)) => {
                self.next_token()?;
                let op = UnaryOperator::get_operator(p).unwrap();
                return Ok(Tree::UnaryOp { operator: op, expression: Box::new(self.parse_primary()?) });
            }
            Token::Punctuation(Punctuation::LCurly) => {
                // parse table initializer - keys and values
                let mut pairs: Vec<(Box<Tree>, Box<Tree>)> = vec![];

                self.next_token()?;

                while self.current_token != Token::Punctuation(Punctuation::RCurly) {
                    let index: Tree = match self.current_token.clone() {
                        Token::Identifier(name) => {
                            self.next_token()?;
                            self.get_string_literal(&name)
                        },
                        Token::String(name) => {
                            self.next_token()?;
                            self.get_string_literal(&name)
                        },
                        Token::Punctuation(Punctuation::LSquare) => {
                            self.next_token()?;
                            let index = self.parse_expression()?;
                            self.expect_punctuation(Punctuation::RSquare)?;
                            index
                        },
                        _ => return self.unexpected_token(self.previous_position(), self.current_token.clone())
                    };

                    self.expect_punctuation(Punctuation::Assign)?;

                    let value = self.parse_expression()?;

                    pairs.push((Box::new(index), Box::new(value)));

                    if self.current_token == Token::Punctuation(Punctuation::Comma) {
                        self.next_token()?;
                    } else if self.current_token != Token::Punctuation(Punctuation::RCurly) {
                        return self.unexpected_token(self.previous_position(), self.current_token.clone());
                    }
                }

                self.next_token()?;

                return Ok(Tree::CreateTable { init_values: pairs });
            }
            _ => return self.parse_index_or_call(None)
        };
        self.next_token()?;
        result
    }

    /// Parses an object index or a function call.
    /// Both of these can be present in either expressions or statements, so this function is used
    /// in both of them.
    fn parse_index_or_call(&mut self, prev_object: Option<Tree>) -> Result<Tree, SyntaxError> {
        let position = self.previous_position();
        let object: Tree = if let Some(obj) = prev_object {
            obj
        } else {
            match self.current_token.clone() {
                Token::Identifier(name) => {
                    self.next_token()?;
                    if let Some(binding) = self.get_binding(&name) {
                        Ok(Tree::BindingValue(binding))
                    } else {
                        Err(SyntaxError {
                            error: SyntaxErrorKind::UnresolvedName(name),
                            source_name: self.source_name(),
                            position,
                        })
                    }
                }
                Token::Punctuation(Punctuation::LParen) => {
                    self.next_token()?;
                    let expr = self.parse_expression()?;
                    self.expect_punctuation(Punctuation::RParen)?;
                    Ok(expr)
                }
                _ => {
                    return self.unexpected_token(position, self.current_token.clone());
                }
            }?
        };

        match self.current_token {
            Token::Punctuation(Punctuation::LParen) => {
                // function call
                let mut parameters: Vec<Box<Tree>> = vec![];
                self.next_token()?;
                if self.current_token != Token::Punctuation(Punctuation::RParen) {
                    while {
                        parameters.push(Box::new(self.parse_expression()?));
                        if self.current_token == Token::Punctuation(Punctuation::Comma) {
                            self.next_token()?;
                            true
                        } else {
                            false
                        }
                    } {}
                }
                self.expect_punctuation(Punctuation::RParen)?;
                return self.parse_index_or_call(Some(Tree::FunctionCall {
                    function: Box::from(object),
                    parameters,
                }));
            }
            Token::Punctuation(Punctuation::Dot) => {
                // member index
                self.next_token()?;

                let name = self.expect_identifier()?;
                let index = self.get_string_literal(&name);

                return self.parse_index_or_call(Some(Tree::ObjectIndex {
                    object: Box::new(object),
                    index: Box::new(index)
                }));
            }
            Token::Punctuation(Punctuation::LSquare) => {
                // expression index
                self.next_token()?;

                let index = self.parse_expression()?;

                self.expect_punctuation(Punctuation::RSquare)?;

                return self.parse_index_or_call(Some(Tree::ObjectIndex {
                    object: Box::new(object),
                    index: Box::new(index)
                }));
            }
            _ => Ok(object)
        }
    }

    /// Parses an infix expression using the shunting yard algorithm.
    fn parse_infix_expression(&mut self, first_primary: Tree) -> Result<Tree, SyntaxError> {
        #[derive(Debug)]
        enum InfixItem {
            Primary(Tree),
            Operator(BinaryOperator),
        }

        let mut output_queue = vec![InfixItem::Primary(first_primary)];
        let mut operator_stack: Vec<BinaryOperator> = vec![];

        while let Some(operator) = BinaryOperator::get_operator(&self.current_token) {
            // pop all operators whose precedence is greater than or equal to the current operator
            while {
                if let Some(last_operator) = operator_stack.last() {
                    if last_operator.precedence() >= operator.precedence() {
                        true
                    } else { false }
                } else { false }
            } {
                output_queue.push(InfixItem::Operator(operator_stack.pop().unwrap()))
            }

            operator_stack.push(operator);
            self.next_token()?;
            output_queue.push(InfixItem::Primary(self.parse_primary()?));
        }

        while !operator_stack.is_empty() {
            output_queue.push(InfixItem::Operator(operator_stack.pop().unwrap()));
        }

        let mut final_stack: Vec<Tree> = vec![];
        output_queue.reverse();
        while !output_queue.is_empty() {
            let item = output_queue.pop().expect(INFIX_ERROR_MESSAGE);
            match item {
                InfixItem::Primary(primary) => {
                    final_stack.push(primary);
                }
                InfixItem::Operator(operator) => {
                    let rhs = final_stack.pop().expect(INFIX_ERROR_MESSAGE);
                    let lhs = final_stack.pop().expect(INFIX_ERROR_MESSAGE);
                    final_stack.push(Tree::BinaryOp {
                        operator,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    });
                }
            }
        }

        let final_item = final_stack.pop().expect(INFIX_ERROR_MESSAGE);

        Ok(final_item)
    }

    fn parse_expression(&mut self) -> Result<Tree, SyntaxError> {
        let lhs = self.parse_primary()?;
        self.parse_infix_expression(lhs)
    }

    fn parse_if_part(&mut self) -> Result<IfPart, SyntaxError> {
        let condition = Box::new(self.parse_expression()?);
        let body = Box::new(self.parse_block()?);
        Ok(IfPart { condition, body })
    }

    fn parse_statement(&mut self) -> Result<Tree, SyntaxError> {
        let position = self.position();

        if let Token::Keyword(keyword) = self.current_token {
            match keyword {
                Keyword::If => {
                    self.next_token()?;

                    let true_part: IfPart = self.parse_if_part()?;

                    let mut elseifs: Vec<IfPart> = vec![];
                    while let Token::Keyword(Keyword::ElseIf) = self.current_token {
                        self.next_token()?;

                        elseifs.push(self.parse_if_part()?);
                    }

                    let else_body = 
                    if let Token::Keyword(Keyword::Else) = self.current_token {
                        self.next_token()?;
                        Some(Box::new(self.parse_block()?))
                    } else { None };

                    return Ok(Tree::IfTree { true_part, elseifs, else_body })
                },

                Keyword::While => {
                    self.next_token()?;

                    let condition = self.parse_expression()?;
                    let body = self.parse_block()?;

                    return Ok(Tree::WhileTree { condition: Box::new(condition), body: Box::new(body) })
                }

                Keyword::Var => {
                    self.next_token()?;
                    
                    let name = self.expect_identifier()?;
                    self.expect_punctuation(Punctuation::Assign)?;
                    let value = self.parse_expression()?;
                    
                    let last_scope = self.binding_scopes.last_mut().unwrap();
                    let location = last_scope.last_local;
                    last_scope.bindings.insert(name, Binding::Local(location));
                    last_scope.last_local += 1;
                    last_scope.stack_size = max(last_scope.stack_size, last_scope.last_local);
                    
                    return Ok(Tree::Assignment {
                        target: Box::new(Tree::BindingValue(Binding::Local(location))),
                        value: Box::new(value)
                    })
                },
                _ => return self.unexpected_token(position, self.current_token.clone())
            }
        }

        let index_or_call = self.parse_index_or_call(None)?;
        if matches!(index_or_call, Tree::FunctionCall {..}) {
            Ok(index_or_call)
        } else {
            self.expect_punctuation(Punctuation::Assign)?;
            let value = self.parse_expression()?;
            Ok(Tree::Assignment {
                target: Box::new(index_or_call),
                value: Box::new(value),
            })
        }
    }

    fn parse_block(&mut self) -> Result<Tree, SyntaxError> {
        self.expect_punctuation(Punctuation::LCurly)?;

        self.binding_scopes.push(
            if let Some(last_scope) = self.binding_scopes.last() {
                Scope{
                    bindings: HashMap::new(),
                    stack_size: last_scope.stack_size,
                    last_local: last_scope.last_local,
                }
            } else {
                Scope {
                    bindings: HashMap::new(),
                    stack_size: 0,
                    last_local: 0,
                }
            }
        );

        let mut result: Vec<Tree> = vec![];
        while self.current_token != Token::Punctuation(Punctuation::RCurly) {
            let statement = self.parse_statement()?;
            result.push(statement);
            if let Token::Punctuation(Punctuation::Semicolon) = self.current_token {
                self.next_token()?;
            }
            //TODO break after return statement
        }
        self.expect_punctuation(Punctuation::RCurly)?;

        let last_stack_size = self.binding_scopes.last().unwrap().stack_size;

        self.binding_scopes.pop();

        if let Some(last_scope) = self.binding_scopes.last_mut() {
            last_scope.stack_size = max(last_scope.stack_size, last_stack_size);
        }

        Ok(Tree::Block{
            statements: result,
            stack_size: last_stack_size,
        })
    }

    pub fn parse_file(&mut self) -> Result<Program, SyntaxError> {
        self.next_token()?;

        let mut function_names: HashMap<String, usize> = HashMap::new();
        let mut functions: Vec<Function> = vec![];
        let mut main_function: Option<Function> = None;

        while !self.lexer.reached_end() {
            let name_position = self.position();
            let token = self.current_token.clone();
            match &token {
                Token::Keyword(Keyword::Func) => {
                    self.next_token()?;
                    let name = self.expect_identifier()?;
                    self.expect_punctuation(Punctuation::LParen)?;
                    //TODO parameters
                    self.expect_punctuation(Punctuation::RParen)?;
                    let block = self.parse_block()?;
                    if let Tree::Block { statements, stack_size } = block {
                        let mut function = Function {
                            code: statements.get_bytecode(),
                            stack_size
                        };
                        if !matches!(function.code.last(), Some(Bytecode::Return(_))) {
                            function.code.push(Bytecode::Return(false));
                        }
                        if name == "main" {
                            main_function = Some(function);
                        } else {
                            if let Some(_) = function_names.get(&*name) {
                                return Err(SyntaxError {
                                    error: SyntaxErrorKind::DuplicateFunctionDefinition(name.clone()),
                                    source_name: self.source_name(),
                                    position: name_position,
                                });
                            }
                            function_names.insert(name.clone(), functions.len());
                            functions.push(function);
                        }

                    }
                }
                _ => return self.unexpected_token(self.position(), token)
            }
        }

        if let Some(func) = main_function {
            Ok(Program {
                functions,
                main_function: func,
                string_literals: self.string_literals.clone()
            })
        } else {
            Err(SyntaxError {
                error: SyntaxErrorKind::MissingMainFunction,
                source_name: self.source_name(),
                position: self.position(),
            })
        }
    }

    pub fn parse_expression_program(&mut self) -> Result<Program, SyntaxError> {
        self.next_token()?;
        let body = self.parse_expression()?;
        Ok(Program {
            functions: vec![],
            main_function: {
                let mut bytecode = body.get_bytecode();
                bytecode.push(Bytecode::Return(true));
                Function { code: bytecode, stack_size: 0 }
            },
            string_literals: vec![],
        })
    }
}