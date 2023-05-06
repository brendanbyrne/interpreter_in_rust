use crate::parser::lexer::Token;

pub enum Expression {
    None,
}

pub enum Statement {
    Let {
	name: String,
	expression: Expression,
    },
}

pub struct Program {
    pub statements: Vec<Statement>
}

impl Program {
    pub fn new() -> Self {
	Program{statements: Vec::new()}
    }
}
