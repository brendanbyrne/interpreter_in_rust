//! Powers the eval portion of the REPL cycle

use std::cell::RefCell;
use std::rc::Rc;

mod environment;
use environment::{Env, Object, FALSE, NOOP, TRUE};

mod error;
use error::{Error, Result};

use crate::parser::{ast, Program};

#[cfg(test)]
mod tests;

/// Contains the state of the execuated program
pub struct Evaluator {
    env: Rc<RefCell<Env>>,
}

impl Evaluator {
    /// Returns an initialized Evaluator
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Env::default())),
        }
    }

    /// Evaluate a program
    pub fn eval(&mut self, program: Program) -> Result<Object> {
        self.statements(program.statements)
    }

    /// Evaluate the statements for the given environment
    fn statements(&mut self, statements: Vec<Box<ast::Statement>>) -> Result<Object> {
        let mut obj = NOOP;
        for statement in statements {
            obj = self.statement(*statement)?;
            if let Object::Return(return_obj) = obj {
                return Ok(*return_obj);
            }
        }
        Ok(obj)
    }

    fn statement(&mut self, statement: ast::Statement) -> Result<Object> {
        match statement {
            ast::Statement::Expression(expr) => self.expression(expr),
            ast::Statement::Block(statements) => self.statements(statements),
            ast::Statement::Return(expr) => Ok(Object::Return(Box::new(self.expression(expr)?))),
            ast::Statement::Let(id, expr) => {
                let obj = self.expression(expr)?;
                self.env.borrow_mut().set(id, obj);
                Ok(NOOP)
            }
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
                Ok(Evaluator::prefix(op, obj)?)
            }
            ast::Expression::Infix(op, lhs, rhs) => {
                let lhs_obj = self.expression(*lhs)?;
                let rhs_obj = self.expression(*rhs)?;
                Ok(Evaluator::infix(op, lhs_obj, rhs_obj)?)
            }
            ast::Expression::If(condition, if_true) => {
                if environment::object::is_truthy(&self.expression(*condition)?) {
                    return Ok(self.statement(*if_true)?);
                }
                Ok(NOOP)
            }
            ast::Expression::IfElse(condition, if_true, if_false) => {
                let mut obj = self.expression(*condition)?;
                if environment::object::is_truthy(&obj) {
                    obj = self.statement(*if_true)?;
                } else {
                    obj = self.statement(*if_false)?;
                }
                Ok(obj)
            }
            ast::Expression::Identifier(id) => self.env.borrow().get(&id),
            ast::Expression::Function(args, body) => Ok(Object::Function(
                args.iter()
                    .map(|a| (*a).to_string())
                    .collect::<Vec<String>>(),
                *body,
                Rc::clone(&self.env),
            )),
            ast::Expression::Call(id, arg_exprs) => {
                let func = self.expression(*id)?;
                self.call_function(func, arg_exprs)
            }
        }
    }

    fn call_function(
        &mut self,
        func: Object,
        arg_exprs: Vec<Box<ast::Expression>>,
    ) -> Result<Object> {
        if let Object::Function(arg_names, body, env) = func {
            if arg_names.len() != arg_exprs.len() {
                return Err(Error::WrongNumberArgs(arg_names.len(), arg_exprs.len()));
            }

            let func_env = Rc::new(RefCell::new(Env::new_with_parent(Rc::clone(&env))));
            for (n, e) in arg_names.into_iter().zip(arg_exprs) {
                let expr = self.expression(*e)?;
                func_env.borrow_mut().set(n, expr);
            }

            return Evaluator { env: func_env }.statement(body);
        }
        panic!("Expected Object::Function, got: {}", func);
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
