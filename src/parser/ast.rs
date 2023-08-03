//! The abstract syntax tree Monkey

use std::fmt;

/// Operators that support the `foo operator bar` format
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InfixOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    Equal,
    NotEqual,
    Call,
}

impl fmt::Display for InfixOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use InfixOperator::*;
        let operator = match self {
            Plus => "+",
            Minus => "-",
            Multiply => "*",
            Divide => "/",
            LessThan => "<",
            GreaterThan => ">",
            Equal => "==",
            NotEqual => "!=",
            Call => panic!("It shouldn't be possible to print a Call this way."),
        };
        write!(f, "{}", operator)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PrefixOperator {
    Negate,
    Not,
}

impl fmt::Display for PrefixOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use PrefixOperator::*;
        let operator = match self {
            Negate => "-",
            Not => "!",
        };
        write!(f, "{}", operator)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    Identifier(String),
    Int(i128),
    Prefix(PrefixOperator, Box<Expression>),
    Infix(InfixOperator, Box<Expression>, Box<Expression>),
    Bool(bool),
    If(Box<Expression>, Box<Statement>),
    IfElse(Box<Expression>, Box<Statement>, Box<Statement>),
    Function(Vec<Box<Expression>>, Box<Statement>),
    Call(Box<Expression>, Vec<Box<Expression>>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expression::*;
        let expression = match self {
            Identifier(name) => name.clone(),
            Int(value) => format!("{}", value),
            Prefix(op, expression) => format!("({}{})", op, *expression),
            Infix(op, lhs, rhs) => format!("({} {} {})", *lhs, op, *rhs),
            Bool(value) => format!("{}", value),
            If(cond, if_true) => format!("if ({}) {}", *cond, *if_true),
            IfElse(cond, if_true, if_false) => {
                format!("if ({}) {} else {}", *cond, *if_true, *if_false)
            }
            Function(params, body) => {
                format!(
                    "fn({}) {}",
                    params
                        .iter()
                        .map(|e| (*e).to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                    body
                )
            }
            Call(name, args) => {
                format!(
                    "{}({})",
                    *name,
                    args.iter()
                        .map(|e| (*e).to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        };
        write!(f, "{}", expression)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    Let(String, Expression),
    Return(Expression),
    Expression(Expression),
    Block(Vec<Box<Statement>>),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Statement::*;
        let statement = match self {
            Let(name, expression) => format!("let {} = {};", name, expression),
            Return(expression) => format!("return {};", expression),
            Expression(expression) => format!("{};", expression),
            Block(statements) => {
                format!(
                    "{{\n{}\n}}",
                    statements
                        .iter()
                        .map(|s| (*s).to_string())
                        .collect::<Vec<String>>()
                        .join("\n\t")
                )
            }
        };
        write!(f, "{}", statement)
    }
}

pub struct Program {
    // Vec<Box<>> so field can call the same functions as a Statement::Block
    pub statements: Vec<Box<Statement>>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(|s| (*s).to_string())
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn to_string() {
        let mut program = Program::new();
        program.statements.push(Box::new(Statement::Let(
            "foo".to_owned(),
            Expression::Int(5),
        )));

        assert_eq!("let foo = 5;".to_owned(), program.to_string());
    }
}
