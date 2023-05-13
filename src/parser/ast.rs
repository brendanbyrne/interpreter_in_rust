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

#[derive(Debug, PartialEq)]
pub enum PrefixOperator {
    Negate,
    Not,
}

impl ToString for PrefixOperator {
    fn to_string(&self) -> String {
        use PrefixOperator::*;
        match self {
            Negate => {
                return "-".to_string();
            }
            Not => {
                return "!".to_string();
            }
        }
    }
}

#[derive(Debug)]
pub enum ExpressionType {
    Empty,
    Identifier(String),
    Int(i128),
    Prefix(PrefixOperator, Box<ExpressionType>),
}

impl ToString for ExpressionType {
    fn to_string(&self) -> String {
        use ExpressionType::*;
        match self {
            Empty => {
                return "".to_string();
            }
            Identifier(name) => {
                return name.clone();
            }
            Int(value) => {
                return format!("{}", value);
            }
            Prefix(operator, expression) => {
                return format!("({}{})", operator.to_string(), (*expression).to_string());
            }
        }
    }
}

#[derive(Debug)]
pub enum StatementType {
    Let {
        name: String,
        expression: ExpressionType,
    },
    Return(ExpressionType),
    Expression(ExpressionType),
}

impl ToString for StatementType {
    fn to_string(&self) -> String {
        use StatementType::*;
        let mut statement = match self {
            Let { name, expression } => {
                format!("let {} = {}", name, expression.to_string())
            }
            Return(expression) => {
                format!("return {}", expression.to_string())
            }
            Expression(expression) => {
                format!("{}", expression.to_string())
            }
        };
        statement.push(';');
        statement
    }
}

pub struct Program {
    pub statements: Vec<StatementType>,
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
        program.statements.push(StatementType::Let {
            name: "foo".to_string(),
            expression: ExpressionType::Int(5),
        });

        assert_eq!("let foo = 5;".to_string(), program.to_string());
    }
}
