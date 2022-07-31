use std::fmt::{Display, Formatter};

#[derive(Copy, Clone)]
pub struct Position {
    pub line: i64,
    pub column: i64,
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}