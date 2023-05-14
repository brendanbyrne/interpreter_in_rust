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

#[derive(Debug)]
pub enum ExpressionType {
    Empty,
    Identifier(String),
    Int(i128),
    Prefix(PrefixOperator, Box<ExpressionType>),
    Infix(Box<ExpressionType>, InfixOperator, Box<ExpressionType>),
}

impl ToString for ExpressionType {
    fn to_string(&self) -> String {
        use ExpressionType::*;
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
        };
        string
    }
}

#[derive(Debug)]
pub enum StatementType {
    Let(String, ExpressionType),
    Return(ExpressionType),
    Expression(ExpressionType),
}

impl ToString for StatementType {
    fn to_string(&self) -> String {
        use StatementType::*;
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
        program.statements.push(StatementType::Let(
            "foo".to_string(),
            ExpressionType::Int(5),
        ));

        assert_eq!("let foo = 5;".to_string(), program.to_string());
    }
}
