//! Powers the eval portion of the REPL cycle

mod error;
use error::{Error, Result};

use crate::parser::{ast, Program};

mod object;
use object::{Object, FALSE, NULL, TRUE};

/// Contains the state of the execuated program
pub struct Evaluator {}

impl Evaluator {
    /// Returns an initialize Evaluator
    pub fn new() -> Self {
        Evaluator {}
    }

    /// Evaluate a program
    pub fn eval(&mut self, program: Program) -> Result<Object> {
        let mut obj = NULL;
        for statement in program.statements {
            obj = self.statement(statement)?;
            if let Object::Return(return_obj) = obj {
                return Ok(*return_obj);
            }
        }
        return Ok(obj);
    }

    fn statement(&mut self, statement: ast::Statement) -> Result<Object> {
        match statement {
            ast::Statement::Expression(expr) => self.expression(expr),
            ast::Statement::Block(statements) => {
                // QUESTION: Is there a way to make this drier?
                // Vec<Box<T>> -> Vec<T> or something
                let mut obj = NULL;
                for statement in statements {
                    obj = self.statement(*statement)?;
                    if let Object::Return(return_obj) = obj {
                        return Ok(*return_obj);
                    }
                }
                Ok(obj)
            }
            ast::Statement::Return(expr) => Ok(Object::Return(Box::new(self.expression(expr)?))),
            _ => Err(Error::UnhandledStatement(statement)),
        }
    }

    fn expression(&mut self, expression: ast::Expression) -> Result<Object> {
        match expression {
            ast::Expression::Int(value) => Ok(Object::Int(value)),
            ast::Expression::Bool(value) => {
                if value {
                    Ok(TRUE)
                } else {
                    Ok(FALSE)
                }
            }
            ast::Expression::Prefix(op, rhs) => {
                let obj = self.expression(*rhs)?;
                Ok(self.prefix(op, obj)?)
            }
            ast::Expression::Infix(op, lhs, rhs) => {
                let lhs_obj = self.expression(*lhs)?;
                let rhs_obj = self.expression(*rhs)?;
                Ok(self.infix(op, lhs_obj, rhs_obj)?)
            }
            ast::Expression::If(condition, if_true) => {
                if object::is_truthy(&self.expression(*condition)?) {
                    return Ok(self.statement(*if_true)?);
                }
                Ok(NULL)
            }
            ast::Expression::IfElse(condition, if_true, if_false) => {
                let mut obj = self.expression(*condition)?;
                if object::is_truthy(&obj) {
                    obj = self.statement(*if_true)?;
                } else {
                    obj = self.statement(*if_false)?;
                }
                Ok(obj)
            }
            _ => Err(Error::UnhandledExpression(expression)),
        }
    }

    fn prefix(&mut self, op: ast::PrefixOperator, rhs: Object) -> Result<Object> {
        match op {
            ast::PrefixOperator::Not => Ok(self.not(rhs)?),
            ast::PrefixOperator::Negate => {
                let obj = self.negate(rhs)?;
                Ok(obj)
            }
        }
    }

    fn not(&mut self, rhs: Object) -> Result<Object> {
        match rhs {
            TRUE => Ok(FALSE),
            FALSE => Ok(TRUE),
            NULL => Ok(TRUE),
            Object::Int(value) => {
                if value == 0 {
                    Ok(TRUE)
                } else {
                    Ok(FALSE)
                }
            }
            Object::Return(obj) => Err(Error::UnexpectedReturn(*obj)),
        }
    }

    fn negate(&mut self, rhs: Object) -> Result<Object> {
        match rhs {
            Object::Int(value) => Ok(Object::Int(-value)),
            _ => Err(Error::UnsupportedNegate(rhs)),
        }
    }

    fn infix(&mut self, op: ast::InfixOperator, lhs: Object, rhs: Object) -> Result<Object> {
        use ast::InfixOperator::*;
        match op {
            Equal => Ok(Object::Bool(lhs == rhs)),
            NotEqual => Ok(Object::Bool(lhs == rhs)),
            Call => panic!("This path should never be executed."),
            _ => Ok(self.infix_math(op, lhs, rhs)?),
        }
    }

    fn infix_math(
        &mut self,
        op: ast::InfixOperator,
        lhs_obj: Object,
        rhs_obj: Object,
    ) -> Result<Object> {
        if let Some((lhs, rhs)) = object::get_infix_ints(lhs_obj.clone(), rhs_obj.clone()) {
            use ast::InfixOperator::*;
            match op {
                Plus => Ok(Object::Int(lhs + rhs)),
                Minus => Ok(Object::Int(lhs - rhs)),
                Multiply => Ok(Object::Int(lhs * rhs)),
                Divide => Ok(Object::Int(lhs / rhs)),
                LessThan => Ok(Object::Bool(lhs < rhs)),
                GreaterThan => Ok(Object::Bool(lhs > rhs)),
                _ => panic!("This path should never be executed."),
            }
        } else {
            Err(Error::InfixTypeMismatch(op, lhs_obj, rhs_obj))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_program;

    #[test]
    fn eval() {
        struct TestCase<'a> {
            input: &'a str,
            expected_obj: Object,
        }

        let test_cases = vec![
            TestCase {
                input: "5",
                expected_obj: Object::Int(5),
            },
            TestCase {
                input: "10",
                expected_obj: Object::Int(10),
            },
            TestCase {
                input: "true",
                expected_obj: TRUE,
            },
            TestCase {
                input: "false",
                expected_obj: FALSE,
            },
            TestCase {
                input: "!true",
                expected_obj: FALSE,
            },
            TestCase {
                input: "!false",
                expected_obj: TRUE,
            },
            TestCase {
                input: "!5",
                expected_obj: FALSE,
            },
            TestCase {
                input: "!!true",
                expected_obj: TRUE,
            },
            TestCase {
                input: "!!false",
                expected_obj: FALSE,
            },
            TestCase {
                input: "!!5",
                expected_obj: TRUE,
            },
            TestCase {
                input: "-5",
                expected_obj: Object::Int(-5),
            },
            TestCase {
                input: "--5",
                expected_obj: Object::Int(5),
            },
            TestCase {
                input: "5 + 5 + 5 + 5 - 10",
                expected_obj: Object::Int(10),
            },
            TestCase {
                input: "2 * 2 * 2 * 2 * 2",
                expected_obj: Object::Int(32),
            },
            TestCase {
                input: "-50 + 100 + -50",
                expected_obj: Object::Int(0),
            },
            TestCase {
                input: "5 * 2 + 10",
                expected_obj: Object::Int(20),
            },
            TestCase {
                input: "5 + 2 * 10",
                expected_obj: Object::Int(25),
            },
            TestCase {
                input: "20 + 2 * -10",
                expected_obj: Object::Int(0),
            },
            TestCase {
                input: "50 / 2 * 2 + 10",
                expected_obj: Object::Int(60),
            },
            TestCase {
                input: "2 * (5 + 10)",
                expected_obj: Object::Int(30),
            },
            TestCase {
                input: "3 * 3 * 3 + 10",
                expected_obj: Object::Int(37),
            },
            TestCase {
                input: "3 * (3 * 3) + 10",
                expected_obj: Object::Int(37),
            },
            TestCase {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
                expected_obj: Object::Int(50),
            },
            TestCase {
                input: "if (true) { 10 }",
                expected_obj: Object::Int(10),
            },
            TestCase {
                input: "if (false) { 10 }",
                expected_obj: NULL,
            },
            TestCase {
                input: "if (1) { 10 }",
                expected_obj: Object::Int(10),
            },
            TestCase {
                input: "if (1 < 2) { 10 }",
                expected_obj: Object::Int(10),
            },
            TestCase {
                input: "if (1 > 2) { 10 }",
                expected_obj: NULL,
            },
            TestCase {
                input: "if (1 > 2) { 10 } else { 20 }",
                expected_obj: Object::Int(20),
            },
            TestCase {
                input: "if (1 < 2) { 10 } else { 20 }",
                expected_obj: Object::Int(10),
            },
            TestCase {
                input: "return 10;",
                expected_obj: Object::Int(10),
            },
            TestCase {
                input: "return 10; 9;",
                expected_obj: Object::Int(10),
            },
            TestCase {
                input: "return 2 * 5;",
                expected_obj: Object::Int(10),
            },
            TestCase {
                input: "9; return 2 * 5; 9;",
                expected_obj: Object::Int(10),
            },
        ];

        for test_case in test_cases {
            let program = parse_program(test_case.input).unwrap();
            let mut evaluator = Evaluator::new();

            let obj = evaluator.eval(program).unwrap();
            assert_eq!(obj, test_case.expected_obj);
        }
    }
}
