#[derive(Debug, PartialEq)]
pub enum InfixOperator {
    Plus,
    Minus,
    Star,
    Slash,
    LessThan,
    GreaterThan,
    Equal,
    NotEqual,
}

impl ToString for InfixOperator {
    fn to_string(&self) -> String {
        use InfixOperator::*;
        let value = match self {
            Plus => "+",
            Minus => "-",
            Star => "*",
            Slash => "/",
            LessThan => "<",
            GreaterThan => ">",
            Equal => "==",
            NotEqual => "!=",
        };
        value.to_string()
    }
}

#[derive(Debug, PartialEq)]
pub enum PrefixOperator {
    Negate,
    Not,
}

impl ToString for PrefixOperator {
    fn to_string(&self) -> String {
        use PrefixOperator::*;
        let value = match self {
            Negate => "-",
            Not => "!",
        };
        value.to_string()
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Empty,
    Identifier(String),
    Int(i128),
    Prefix(PrefixOperator, Box<Expression>),
    Infix(Box<Expression>, InfixOperator, Box<Expression>),
    Boolean(bool),
    If(Box<Expression>, Box<Statement>, Box<Statement>),
}

impl ToString for Expression {
    fn to_string(&self) -> String {
        use Expression::*;
        let string = match self {
            Empty => "".to_string(),
            Identifier(name) => name.clone(),
            Int(value) => format!("{}", value),
            Prefix(op, expression) => {
                format!("({}{})", op.to_string(), (*expression).to_string())
            }
            Infix(lhs, op, rhs) => {
                format!(
                    "({} {} {})",
                    (*lhs).to_string(),
                    op.to_string(),
                    (*rhs).to_string()
                )
            }
            Boolean(value) => format!("{}", value),
            _ => "Not implemented".to_string(),
        };
        string
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(String, Expression),
    Return(Expression),
    Expression(Expression),
    Block(Vec<Box<Statement>>),
}

impl ToString for Statement {
    fn to_string(&self) -> String {
        use Statement::*;
        let mut statement = match self {
            Let(name, expression) => {
                format!("let {} = {}", name, expression.to_string())
            }
            Return(expression) => {
                format!("return {}", expression.to_string())
            }
            Expression(expression) => {
                format!("{}", expression.to_string())
            }
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
        statement
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}

impl ToString for Program {
    fn to_string(&self) -> String {
        self.statements
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join("\n")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn to_string() {
        let mut program = Program::new();
        program
            .statements
            .push(Statement::Let("foo".to_string(), Expression::Int(5)));

        assert_eq!("let foo = 5".to_string(), program.to_string());
    }
}
