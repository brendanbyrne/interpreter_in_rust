//! Powers the eval portion of the REPL cycle

use std::ops::Deref;
use std::rc::Rc;

mod environment;
use environment::{Env, Object, FALSE, NOOP, NULL, TRUE};

mod error;
use error::{Error, Result};

use crate::parser::{ast, Program};

/// Contains the state of the execuated program
pub struct Evaluator {
    env: Env,
}

impl Evaluator {
    /// Returns an initialized Evaluator
    pub fn new() -> Self {
        Self { env: Env::new() }
    }

    /// Evaluate a program
    pub fn eval(&mut self, program: Program) -> Result<Object> {
        Evaluator::statements(program.statements, &mut self.env)
    }

    /// Evaluate the statements for the given environment
    fn statements(statements: Vec<Box<ast::Statement>>, env: &mut Env) -> Result<Object> {
        let mut obj = NULL;
        for statement in statements {
            obj = Evaluator::statement(*statement, env)?;
            if let Object::Return(return_obj) = obj {
                return Ok(*return_obj);
            }
        }
        Ok(obj)
    }

    fn statement(statement: ast::Statement, env: &mut Env) -> Result<Object> {
        match statement {
            ast::Statement::Expression(expr) => Evaluator::expression(expr, env),
            ast::Statement::Block(statements) => Evaluator::statements(statements, env),
            ast::Statement::Return(expr) => {
                Ok(Object::Return(Box::new(Evaluator::expression(expr, env)?)))
            }
            ast::Statement::Let(id, expr) => {
                let obj = Evaluator::expression(expr, env)?;
                env.set(id, Rc::new(obj));
                Ok(NOOP)
            }
        }
    }

    fn expression(expression: ast::Expression, env: &mut Env) -> Result<Object> {
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
                let obj = Evaluator::expression(*rhs, env)?;
                Ok(Evaluator::prefix(op, obj)?)
            }
            ast::Expression::Infix(op, lhs, rhs) => {
                let lhs_obj = Evaluator::expression(*lhs, env)?;
                let rhs_obj = Evaluator::expression(*rhs, env)?;
                Ok(Evaluator::infix(op, lhs_obj, rhs_obj)?)
            }
            ast::Expression::If(condition, if_true) => {
                if environment::object::is_truthy(&Evaluator::expression(*condition, env)?) {
                    return Ok(Evaluator::statement(*if_true, env)?);
                }
                Ok(NOOP)
            }
            ast::Expression::IfElse(condition, if_true, if_false) => {
                let mut obj = Evaluator::expression(*condition, env)?;
                if environment::object::is_truthy(&obj) {
                    obj = Evaluator::statement(*if_true, env)?;
                } else {
                    obj = Evaluator::statement(*if_false, env)?;
                }
                Ok(obj)
            }
            ast::Expression::Identifier(id) => match env.get(id.clone()) {
                Some(rc_obj) => Ok(rc_obj.deref().clone()),
                None => Err(Error::IdNotFound(id)),
            },
            ast::Expression::Function(args, body) => Ok(Object::Function(
                args.iter()
                    .map(|a| (*a).to_string())
                    .collect::<Vec<String>>(),
                *body,
                env.nest(),
            )),
            ast::Expression::Call(id, arg_exprs) => {
                let expr = Evaluator::expression(*id, env)?;

                if let Object::Function(arg_names, body, mut func_env) = expr {
                    if arg_names.len() != arg_exprs.len() {
                        return Err(Error::WrongNumberArgs(arg_names.len(), arg_exprs.len()));
                    }

                    func_env.clear_top_scope(); // TODO: Verify that this is necessary
                    for (n, e) in arg_names.into_iter().zip(arg_exprs) {
                        let expr = Evaluator::expression(*e, env)?;
                        func_env.set(n, Rc::new(expr));
                    }

                    return Evaluator::statement(body, &mut func_env);
                }
                panic!("Expected Object::Function, got: {}", expr);
            }
        }
    }

    fn prefix(op: ast::PrefixOperator, rhs: Object) -> Result<Object> {
        match op {
            ast::PrefixOperator::Not => Ok(Evaluator::not(rhs)?),
            ast::PrefixOperator::Negate => {
                let obj = Evaluator::negate(rhs)?;
                Ok(obj)
            }
        }
    }

    fn not(rhs: Object) -> Result<Object> {
        use Object::*;
        match rhs {
            TRUE => Ok(FALSE),
            FALSE => Ok(TRUE),
            NULL => Ok(TRUE),
            Int(value) => {
                if value == 0 {
                    Ok(TRUE)
                } else {
                    Ok(FALSE)
                }
            }
            Return(obj) => Err(Error::UnexpectedReturn(*obj)),
            NOOP => panic!("Nothing should have the value NOOP"),
            Function(_, _, _) => panic!("Not of a function doesn't mean anything."),
        }
    }

    fn negate(rhs: Object) -> Result<Object> {
        match rhs {
            Object::Int(value) => Ok(Object::Int(-value)),
            _ => Err(Error::UnsupportedNegate(rhs)),
        }
    }

    fn infix(op: ast::InfixOperator, lhs: Object, rhs: Object) -> Result<Object> {
        use ast::InfixOperator::*;
        match op {
            Equal => Ok(Object::Bool(lhs == rhs)),
            NotEqual => Ok(Object::Bool(lhs == rhs)),
            Call => panic!("This path should never be executed."),
            _ => Ok(Evaluator::infix_math(op, lhs, rhs)?),
        }
    }

    fn infix_math(op: ast::InfixOperator, lhs_obj: Object, rhs_obj: Object) -> Result<Object> {
        if let Some((lhs, rhs)) =
            environment::object::get_infix_ints(lhs_obj.clone(), rhs_obj.clone())
        {
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
                expected_obj: NOOP,
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
                expected_obj: NOOP,
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
            TestCase {
                input: "let a = 5; a;",
                expected_obj: Object::Int(5),
            },
            TestCase {
                input: "let a = 5 * 5; a;",
                expected_obj: Object::Int(25),
            },
            TestCase {
                input: "let a = 5; let b = a; b;",
                expected_obj: Object::Int(5),
            },
            TestCase {
                input: "let a = 5; let b = a; let c = a + b + 5; c;",
                expected_obj: Object::Int(15),
            },
            TestCase {
                input: "fn(x) { x + 2; };",
                expected_obj: Object::Function(
                    vec!["x".to_owned()],
                    ast::Statement::Block(vec![Box::new(ast::Statement::Expression(
                        ast::Expression::Infix(
                            ast::InfixOperator::Plus,
                            Box::new(ast::Expression::Identifier("x".to_owned())),
                            Box::new(ast::Expression::Int(2)),
                        ),
                    ))]),
                    Env::new().nest(),
                ),
            },
            TestCase {
                input: "let identity = fn(x) { x; }; identity(5);",
                expected_obj: Object::Int(5),
            },
            TestCase {
                input: "let identity = fn(x) { return x; }; identity(5);",
                expected_obj: Object::Int(5),
            },
            TestCase {
                input: "let double = fn(x) { x * 2; }; double(5);",
                expected_obj: Object::Int(10),
            },
            TestCase {
                input: "let add = fn(x, y) { x + y; }; add(5, 5);",
                expected_obj: Object::Int(10),
            },
            TestCase {
                input: "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                expected_obj: Object::Int(20),
            },
            TestCase {
                input: "
let new_adder = fn(x) { fn(y) { x + y; }; };
let add_two = new_adder(2);
add_two(2);",
                expected_obj: Object::Int(4),
            },
        ];

        for test_case in test_cases {
            let program = parse_program(test_case.input).unwrap();
            let mut evaluator = Evaluator::new();

            let obj = evaluator.eval(program).unwrap();
            assert_eq!(obj, test_case.expected_obj);
        }
    }

    #[test]
    fn errors() {
        struct TestCase<'a> {
            input: &'a str,
            expected_error: Error,
        }

        let test_cases = vec![
            TestCase {
                input: "5 + true;",
                expected_error: Error::InfixTypeMismatch(
                    ast::InfixOperator::Plus,
                    Object::Int(5),
                    Object::Bool(true),
                ),
            },
            TestCase {
                input: "5 + true; 5;",
                expected_error: Error::InfixTypeMismatch(
                    ast::InfixOperator::Plus,
                    Object::Int(5),
                    TRUE,
                ),
            },
            TestCase {
                input: "-true",
                expected_error: Error::UnsupportedNegate(TRUE),
            },
            TestCase {
                input: "true + false;",
                expected_error: Error::InfixTypeMismatch(ast::InfixOperator::Plus, TRUE, FALSE),
            },
            TestCase {
                input: "5; true + false; 5;",
                expected_error: Error::InfixTypeMismatch(ast::InfixOperator::Plus, TRUE, FALSE),
            },
            TestCase {
                input: "if (10 > 1) { true + false; }",
                expected_error: Error::InfixTypeMismatch(ast::InfixOperator::Plus, TRUE, FALSE),
            },
            TestCase {
                input: "
if (10 > 1) {
  if (10 > 1) {
    return true + false;
  }

  return 1;
}",
                expected_error: Error::InfixTypeMismatch(ast::InfixOperator::Plus, TRUE, FALSE),
            },
            TestCase {
                input: "foobar",
                expected_error: Error::IdNotFound("foobar".to_owned()),
            },
        ];

        for test_case in test_cases {
            let program = parse_program(test_case.input).unwrap();
            let mut evaluator = Evaluator::new();
            assert_eq!(
                evaluator.eval(program).unwrap_err(),
                test_case.expected_error
            );
        }
    }
}
