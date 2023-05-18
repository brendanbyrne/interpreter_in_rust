//! Powers the eval portion of the REPL cycle

use crate::parser::{ast, parse_program, Program};

mod object;
use object::Object;

/// Contains the state of the execuated program
pub struct Evaluator {}

impl Evaluator {
    /// Returns an initialize Evaluator
    pub fn new() -> Self {
        Evaluator {}
    }

    /// Evaluate a program
    pub fn eval(&mut self, program: Program) {
        for statement in program.statements {
            self.eval_statement(statement);
        }
    }

    fn eval_statement(&mut self, statement: ast::Statement) -> Object {
        match statement {
            ast::Statement::Expression(expr) => self.eval_expression(expr),
            _ => Object::Null,
        }
    }

    fn eval_expression(&mut self, expression: ast::Expression) -> Object {
        match expression {
            ast::Expression::Int(value) => Object::Int(value),
            _ => Object::Null,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn int() {
        struct TestCase<'a> {
            input: &'a str,
            expected_value: i128,
        }

        let test_cases = vec![
            TestCase {
                input: "5",
                expected_value: 5,
            },
            TestCase {
                input: "10",
                expected_value: 10,
            },
        ];

        for test_case in test_cases {
            let mut program = parse_program(test_case.input).unwrap();
            let mut evaluator = Evaluator::new();

            if let Object::Int(value) = evaluator.eval_statement(program.statements.pop().unwrap())
            {
                assert_eq!(value, test_case.expected_value);
            } else {
                panic!("Failed to eval to Int");
            }
        }
    }
}
