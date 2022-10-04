use std::str::FromStr;

use crate::lang::{Position, SyntaxError, SyntaxErrorKind, Token};
use crate::lang::token::{Keyword, Punctuation};

fn is_name_char(c: char) -> bool {
    c == '_' || c.is_alphanumeric()
}

pub struct Lexer {
    code_iterator: Box<dyn Iterator<Item=char>>,
    source_name: String,
    current_index: usize,
    position: Position,
    previous_position: Position,
    current_char: char,

    reached_end: bool,
}

impl Lexer {
    pub fn new(code_iterator: Box<dyn Iterator<Item=char>>, source_name: String) -> Lexer {
        let mut l = Lexer {
            code_iterator,
            current_index: 0,
            position: Position { line: 0, column: 0, source_name: source_name.clone() },
            previous_position: Position { line: 0, column: 0, source_name: source_name.clone() },
            source_name,
            current_char: '\0',
            reached_end: false,
        };

        l.advance_char();

        l
    }

    fn advance_char(&mut self) {
        if self.reached_end { return; }
        let c = self.code_iterator.next();
        self.position.column += 1;
        match c {
            None => {
                self.reached_end = true;
                self.current_char = '\0';
            }
            Some(c) => {
                if c == '\n' {
                    self.position.line += 1;
                    self.position.column = 0;
                }
                self.current_char = c;
            }
        }
    }

    pub fn reached_end(&self) -> bool {
        self.reached_end
    }
    pub fn source_name(&self) -> String {
        self.source_name.clone()
    }
    pub fn position(&self) -> Position {
        self.position.clone()
    }
    pub fn previous_position(&self) -> Position {
        self.previous_position.clone()
    }

    pub fn next(&mut self) -> Result<Token, SyntaxError> {
        // skip past whitespace and comments
        while !self.reached_end && self.current_char.is_whitespace() {
            self.advance_char();
            if self.current_char == '/' {
                let initial_position = self.position.clone();
                self.advance_char();
                if self.current_char == '/' {
                    // single line comment
                    while !(self.current_char == '\n' || self.reached_end) {
                        self.advance_char();
                    }
                } else if self.current_char == '*' {
                    // multi line comment
                    'multi_comment: loop {
                        self.advance_char();
                        if self.current_char == '*' {
                            self.advance_char();
                            if self.current_char == '/' {
                                self.advance_char();
                                break 'multi_comment;
                            }
                        }
                        if self.reached_end {
                            return Err(SyntaxError {
                                error: SyntaxErrorKind::UnfinishedMultilineComment,
                                position: initial_position,
                            });
                        }
                    }
                } else {
                    return Ok(Token::Punctuation(Punctuation::Slash));
                }
            }
        }

        self.previous_position = self.position.clone();

        if self.reached_end {
            return Ok(Token::EOF);
        }

        let initial_position = self.position.clone();

        if self.current_char == '"' {
            // string
            let mut the_string: String = "".to_string();
            self.advance_char();
            while self.current_char != '"' {
                the_string.push(self.current_char);
                self.advance_char();
                if self.reached_end {
                    return Err(SyntaxError {
                        error: SyntaxErrorKind::UnfinishedString,
                        position: initial_position,
                    });
                }
            }
            self.advance_char();
            Ok(Token::String(the_string))
        } else if self.current_char.is_digit(10) {
            // number
            let mut number_str: String = "".to_string();
            while self.current_char.is_digit(10) || self.current_char == '.' {
                number_str.push(self.current_char);
                self.advance_char();
            }
            let number = number_str.parse::<f64>();
            match number {
                Err(_why) => Err(SyntaxError {
                    error: SyntaxErrorKind::MalformedNumber(number_str),
                    position: initial_position,
                }),
                Ok(num) => Ok(Token::Number(num))
            }
        } else if is_name_char(self.current_char) {
            // identifier or keyword
            let mut the_string: String = "".to_string();
            while is_name_char(self.current_char) {
                the_string.push(self.current_char);
                self.advance_char();
            }
            if let Ok(kw) = Keyword::from_str(the_string.as_str()) {
                Ok(Token::Keyword(kw))
            } else {
                Ok(Token::Identifier(the_string))
            }
        } else if let Ok(op) = Punctuation::from_str(self.current_char.to_string().as_str()) {
            // punctuation
            let mut the_op_string: String = self.current_char.to_string();
            let mut the_op = op;
            self.advance_char();
            // join multi-character tokens
            while let Ok(future_op) = Punctuation::from_str({
                the_op_string.push(self.current_char);
                the_op_string.as_str()
            }) {
                the_op = future_op;
                self.advance_char();
            }
            Ok(Token::Punctuation(the_op))
        } else {
            // unknown character
            Err(SyntaxError {
                error: SyntaxErrorKind::UnknownCharacter(self.current_char),
                position: self.position.clone(),
            })
        }
    }
}

#[cfg(test)]
mod lexer_tests {
    use Token::*;

    use crate::lang::token::Keyword::*;
    use crate::lang::token::Punctuation::*;

    use super::*;

    #[test]
    fn test_lexer() {
        //TODO add other keywords
        let expected_tokens = [Punctuation(Slash), Number(12.5), Punctuation(Asterisk),
            Punctuation(LParen), Punctuation(RParen), Punctuation(Assign),
            Identifier("identW_ow".to_string()), Punctuation(Minus), Punctuation(Plus),
            Punctuation(LEqual), Number(69.0), Punctuation(GEqual), Punctuation(Less),
            Punctuation(Greater), Punctuation(Equal), Punctuation(LCurly), Punctuation(Dot),
            Punctuation(RCurly), Punctuation(LSquare), Punctuation(RSquare), Keyword(If),
            String("wow a string verycool1231 ! !".to_string()), Keyword(If), Number(5.3654),
            Identifier("_opopo1234".to_string()), Identifier("sdfg".to_string()),
            Keyword(True), Keyword(False), Punctuation(NotEqual),
        ];
        let test_code = "\
/ 12.5 * () =identW_ow- + <= 69 >= <> /*asdf asodfh asdufh asd f*/ == {.}[] if \"wow a string verycool1231 ! !\" if //wow comment lafdsfasd asdf \"\"<
5.3654 /*asdf*/ _opopo1234 sdfg true false !=";
        let mut test_lexer = Lexer::new(Box::new(test_code.chars()), "code".to_string());
        for expected in expected_tokens.iter() {
            match test_lexer.next() {
                Ok(token) => {
                    assert_eq!(*expected, token);
                }
                Err(_err) => {
                    panic!();
                }
            }
        }
    }
}