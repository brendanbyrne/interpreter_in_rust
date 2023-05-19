#[derive(Clone, Debug, PartialEq)]
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

impl ToString for InfixOperator {
    fn to_string(&self) -> String {
        use InfixOperator::*;
        let value = match self {
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
        value.to_string()
    }
}

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
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

impl ToString for Expression {
    fn to_string(&self) -> String {
        use Expression::*;
        let string = match self {
            Identifier(name) => name.clone(),
            Int(value) => format!("{}", value),
            Prefix(op, expression) => {
                format!("({}{})", op.to_string(), (*expression).to_string())
            }
            Infix(op, lhs, rhs) => {
                format!(
                    "({} {} {})",
                    (*lhs).to_string(),
                    op.to_string(),
                    (*rhs).to_string()
                )
            }
            Bool(value) => format!("{}", value),
            If(cond, if_true) => {
                format!("if ({}) {}", (*cond).to_string(), (*if_true).to_string())
            }
            IfElse(cond, if_true, if_false) => {
                format!(
                    "if ({}) {} else {}",
                    (*cond).to_string(),
                    (*if_true).to_string(),
                    (*if_false).to_string()
                )
            }
            Function(params, body) => {
                format!(
                    "fn({}) {}",
                    params
                        .iter()
                        .map(|e| (*e).to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                    body.to_string()
                )
            }
            Call(name, args) => {
                format!(
                    "{}({})",
                    (*name).to_string(),
                    args.iter()
                        .map(|e| (*e).to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        };
        string
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Let(String, Expression),
    Return(Expression),
    Expression(Expression),
    Block(Vec<Box<Statement>>),
}

impl ToString for Statement {
    fn to_string(&self) -> String {
        use Statement::*;
        let statement = match self {
            Let(name, expression) => {
                format!("let {} = {};", name, expression.to_string())
            }
            Return(expression) => {
                format!("return {};", expression.to_string())
            }
            Expression(expression) => {
                format!("{};", expression.to_string())
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

        assert_eq!("let foo = 5;".to_string(), program.to_string());
    }
}
