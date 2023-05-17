pub mod lexer;
use lexer::{Lexer, Token};

pub mod ast;

use std::result;
type Result<T> = result::Result<T, String>;

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

#[derive(PartialEq, PartialOrd)]
enum Ordering {
    Lowest,
    Equals,         // == or !=
    LessGreater,    // > or <
    PlusMinus,      // + or -
    MultiplyDivide, // * or /
    Prefix,         // -x or !x
    Call,           // x()
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            cur_token: Token::EOF,
            peek_token: Token::EOF,
            errors: Vec::new(),
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program::new();

        while self.cur_token != Token::EOF {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                Err(msg) => self.errors.push(msg),
            };
            self.next_token();
        }
        program
    }

    fn next_token(&mut self) {
        // QUESTION: This seems weird to me.  Why do I have to clone here?
        // I want to do like a simultaneous exchange like this:
        //          next_token() -> peek -> cur
        //   but all at once
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Result<ast::Statement> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn get_ident_name(&mut self) -> Result<String> {
        if let Token::Ident(name) = self.cur_token.clone() {
            Ok(name)
        } else {
            Err(Parser::peek_error_msg(
                &Token::Ident("<name>".to_owned()),
                &self.cur_token,
            ))
        }
    }

    fn parse_let_statement(&mut self) -> Result<ast::Statement> {
        assert_eq!(self.cur_token, Token::Let);
        self.next_token();

        let name = self.get_ident_name()?;

        self.expect_peek(&Token::Assign)?;

        // TODO: Skipping to semicolon
        while self.cur_token != Token::Semicolon {
            self.next_token();
        }

        Ok(ast::Statement::Let(name, ast::Expression::Empty))
    }

    fn parse_return_statement(&mut self) -> Result<ast::Statement> {
        assert_eq!(self.cur_token, Token::Return);
        self.next_token();

        // TODO: Skipping to semicolon
        while self.cur_token != Token::Semicolon {
            self.next_token();
        }
        Ok(ast::Statement::Return(ast::Expression::Empty))
    }

    fn parse_expression_statement(&mut self) -> Result<ast::Statement> {
        let expression = self.parse_expression(Ordering::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(ast::Statement::Expression(expression))
    }

    fn parse_expression(&mut self, ordering: Ordering) -> Result<ast::Expression> {
        let mut expression = self.parse_prefix()?;

        while self.peek_token != Token::Semicolon && ordering < Parser::precedence(&self.peek_token)
        {
            if let Some(op) = Parser::get_infix_operator(&self.peek_token) {
                self.next_token(); // Move to infix operator
                expression = self.parse_infix_expression(expression, op)?;
            } else {
                return Ok(expression);
            }
        }

        return Ok(expression);
    }

    fn parse_prefix(&mut self) -> Result<ast::Expression> {
        match &self.cur_token {
            Token::Ident(name) => return Ok(ast::Expression::Identifier(name.clone())),
            Token::Int(value) => return self.parse_prefix_int(value.to_string()),
            Token::Bang => return self.parse_prefix_expression(ast::PrefixOperator::Not),
            Token::Minus => return self.parse_prefix_expression(ast::PrefixOperator::Negate),
            Token::True => return Ok(ast::Expression::Boolean(true)),
            Token::False => return Ok(ast::Expression::Boolean(false)),
            Token::LParen => return self.parse_group(),
            Token::If => return self.parse_if(),
            _ => return Err(format!("No prefix parser for {:?}", self.cur_token)),
        };
    }

    fn parse_prefix_int(&mut self, input: String) -> Result<ast::Expression> {
        if let Ok(value) = input.parse::<i128>() {
            Ok(ast::Expression::Int(value))
        } else {
            Err(format!("Could not parse '{}' as an integer", input))
        }
    }

    fn parse_prefix_expression(&mut self, op: ast::PrefixOperator) -> Result<ast::Expression> {
        self.next_token(); // drop operator

        let expression = self.parse_expression(Ordering::Prefix)?;
        Ok(ast::Expression::Prefix(op, Box::new(expression)))
    }

    fn parse_infix_expression(
        &mut self,
        lhs: ast::Expression,
        op: ast::InfixOperator,
    ) -> Result<ast::Expression> {
        let precedence = Parser::precedence(&self.cur_token);
        self.next_token(); // drop operator

        let rhs = self.parse_expression(precedence)?;
        Ok(ast::Expression::Infix(Box::new(lhs), op, Box::new(rhs)))
    }

    fn parse_group(&mut self) -> Result<ast::Expression> {
        assert_eq!(self.cur_token, Token::LParen);
        self.next_token();
        let expression = self.parse_expression(Ordering::Lowest)?;
        self.expect_peek(&Token::RParen)?;
        return Ok(expression);
    }

    fn parse_if(&mut self) -> Result<ast::Expression> {
        self.expect_peek(&Token::LParen)?;
        self.next_token(); // drop LParen, inside of if ( <HERE> )

        let condition = self.parse_expression(Ordering::Lowest)?;

        self.expect_peek(&Token::RParen)?;
        self.expect_peek(&Token::LBrace)?;

        let consequence = self.parse_block_statement()?;

        if self.peek_token == Token::Else {
            assert_eq!(self.cur_token, Token::RBrace);
            self.next_token(); // now is Else
            self.expect_peek(&Token::LBrace)?;
            let alternative = self.parse_block_statement()?;
            return Ok(ast::Expression::IfElse(
                Box::new(condition),
                Box::new(consequence),
                Box::new(alternative),
            ));
        }

        return Ok(ast::Expression::If(
            Box::new(condition),
            Box::new(consequence),
        ));
    }

    fn parse_block_statement(&mut self) -> Result<ast::Statement> {
        assert_eq!(self.cur_token, Token::LBrace);
        self.next_token();

        let mut statements = Vec::new();

        while self.cur_token != Token::RBrace && self.cur_token != Token::EOF {
            match self.parse_statement() {
                Ok(statement) => statements.push(Box::new(statement)),
                Err(msg) => self.errors.push(msg),
            };
            self.next_token();
        }

        Ok(ast::Statement::Block(statements))
    }

    fn expect_peek(&mut self, token: &Token) -> Result<()> {
        if &self.peek_token == token {
            self.next_token();
            Ok(())
        } else {
            Err(Parser::peek_error_msg(token, &self.peek_token))
        }
    }

    fn peek_error_msg(expected: &Token, found: &Token) -> String {
        format!(
            "expected next token to be {:?}, got {:?} instead",
            expected, found
        )
    }

    fn get_infix_operator(token: &Token) -> Option<ast::InfixOperator> {
        match token {
            Token::Equal => return Some(ast::InfixOperator::Equal),
            Token::NotEqual => return Some(ast::InfixOperator::NotEqual),
            Token::LessThan => return Some(ast::InfixOperator::LessThan),
            Token::GreaterThan => return Some(ast::InfixOperator::GreaterThan),
            Token::Plus => return Some(ast::InfixOperator::Plus),
            Token::Minus => return Some(ast::InfixOperator::Minus),
            Token::Asterisk => return Some(ast::InfixOperator::Multiply),
            Token::Slash => return Some(ast::InfixOperator::Divide),
            _ => return None,
        };
    }

    fn precedence(token: &Token) -> Ordering {
        let p = match token {
            Token::Equal => Ordering::Equals,
            Token::NotEqual => Ordering::Equals,
            Token::LessThan => Ordering::LessGreater,
            Token::GreaterThan => Ordering::LessGreater,
            Token::Plus => Ordering::PlusMinus,
            Token::Minus => Ordering::PlusMinus,
            Token::Asterisk => Ordering::MultiplyDivide,
            Token::Slash => Ordering::MultiplyDivide,
            _ => Ordering::Lowest,
        };
        p
    }
}

// QUESTION: Should this go into a separate file?
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn let_statements() {
        let mut parser = make_parser(
            "
let x = 5;
let y = 10;
let foobar = 838383;
",
        );
        let program = parser.parse_program();
        check_parser(&parser);

        assert_eq!(program.statements.len(), 3);

        let tests = vec!["x", "y", "foobar"];

        for (s, t) in program.statements.iter().zip(tests.iter()) {
            if let ast::Statement::Let(name, _) = s {
                assert_eq!(name, t);
            } else {
                panic!("Expected type ast::Statement::Let");
            }
        }
    }

    #[test]
    fn return_statements() {
        let mut parser = make_parser(
            "
return 5;
return 10;
return 993322;
",
        );
        let program = parser.parse_program();
        check_parser(&parser);

        assert_eq!(program.statements.len(), 3);

        for s in program.statements {
            if let ast::Statement::Return(_) = s {
            } else {
                panic!("Expected type ast::Statement::Return");
            }
        }
    }

    #[test]
    fn identifier_expression() {
        let mut parser = make_parser("foobar");

        let mut program = parser.parse_program();
        check_parser(&parser);

        assert_eq!(program.statements.len(), 1);

        if let ast::Statement::Expression(expression) = program.statements.pop().unwrap() {
            if let ast::Expression::Identifier(name) = expression {
                assert_eq!("foobar", name);
            } else {
                panic!("Expected type ast::Expression::Indentifier");
            }
        } else {
            panic!("Expected type ast::Statement::Expression");
        }
    }

    #[test]
    fn integer_literal_expression() {
        let mut parser = make_parser("5");

        let mut program = parser.parse_program();
        check_parser(&parser);

        assert_eq!(program.statements.len(), 1);

        if let ast::Statement::Expression(expression) = program.statements.pop().unwrap() {
            if let ast::Expression::Int(value) = expression {
                assert_eq!(5, value);
            } else {
                panic!("Expected type ast::Expression::Indentifier");
            }
        } else {
            panic!("Expected type ast::Statement::Expression");
        }
    }

    #[test]
    fn prefix_expression() {
        struct TestCase<'a> {
            input: &'a str,
            op: ast::PrefixOperator,
            value: ast::Expression,
        }

        let test_cases = vec![
            TestCase {
                input: "!5",
                op: ast::PrefixOperator::Not,
                value: ast::Expression::Int(5),
            },
            TestCase {
                input: "-15",
                op: ast::PrefixOperator::Negate,
                value: ast::Expression::Int(15),
            },
            TestCase {
                input: "!true",
                op: ast::PrefixOperator::Not,
                value: ast::Expression::Boolean(true),
            },
            TestCase {
                input: "!false",
                op: ast::PrefixOperator::Not,
                value: ast::Expression::Boolean(false),
            },
        ];

        for test_case in test_cases {
            let mut parser = make_parser(test_case.input);
            let mut program = parser.parse_program();
            check_parser(&parser);

            assert_eq!(program.statements.len(), 1);

            // QUESTION: Is there a way to do this without nesting `if let`s like this?
            if let ast::Statement::Expression(expression) = program.statements.pop().unwrap() {
                if let ast::Expression::Prefix(op, nested_expression) = expression {
                    assert_eq!(op, test_case.op);

                    assert_eq!(*nested_expression, test_case.value);
                } else {
                    panic!("Expected type ast::Expression::Prefix");
                }
            } else {
                panic!("Expected type ast::Statement::Expression");
            }
        }
    }

    #[test]
    fn infix_expression() {
        struct TestCase<'a> {
            input: &'a str,
            lhs: ast::Expression,
            rhs: ast::Expression,
            op: ast::InfixOperator,
        }

        let test_cases = vec![
            TestCase {
                input: "5 + 5",
                lhs: ast::Expression::Int(5),
                rhs: ast::Expression::Int(5),
                op: ast::InfixOperator::Plus,
            },
            TestCase {
                input: "5 - 5",
                lhs: ast::Expression::Int(5),
                rhs: ast::Expression::Int(5),
                op: ast::InfixOperator::Minus,
            },
            TestCase {
                input: "5 * 5",
                lhs: ast::Expression::Int(5),
                rhs: ast::Expression::Int(5),
                op: ast::InfixOperator::Multiply,
            },
            TestCase {
                input: "5 / 5",
                lhs: ast::Expression::Int(5),
                rhs: ast::Expression::Int(5),
                op: ast::InfixOperator::Divide,
            },
            TestCase {
                input: "5 > 5",
                lhs: ast::Expression::Int(5),
                rhs: ast::Expression::Int(5),
                op: ast::InfixOperator::GreaterThan,
            },
            TestCase {
                input: "5 < 5",
                lhs: ast::Expression::Int(5),
                rhs: ast::Expression::Int(5),
                op: ast::InfixOperator::LessThan,
            },
            TestCase {
                input: "5 == 5",
                lhs: ast::Expression::Int(5),
                rhs: ast::Expression::Int(5),
                op: ast::InfixOperator::Equal,
            },
            TestCase {
                input: "5 != 5",
                lhs: ast::Expression::Int(5),
                rhs: ast::Expression::Int(5),
                op: ast::InfixOperator::NotEqual,
            },
            TestCase {
                input: "true == true",
                lhs: ast::Expression::Boolean(true),
                rhs: ast::Expression::Boolean(true),
                op: ast::InfixOperator::Equal,
            },
            TestCase {
                input: "true != false",
                lhs: ast::Expression::Boolean(true),
                rhs: ast::Expression::Boolean(false),
                op: ast::InfixOperator::NotEqual,
            },
            TestCase {
                input: "false == false",
                lhs: ast::Expression::Boolean(false),
                rhs: ast::Expression::Boolean(false),
                op: ast::InfixOperator::Equal,
            },
        ];

        for test_case in test_cases {
            let mut parser = make_parser(test_case.input);
            let mut program = parser.parse_program();
            check_parser(&parser);

            assert_eq!(program.statements.len(), 1);

            // QUESTION: Is there a way to do this without nesting `if let`s like this?
            if let ast::Statement::Expression(expression) = program.statements.pop().unwrap() {
                if let ast::Expression::Infix(lhs, op, rhs) = expression {
                    assert_eq!(op, test_case.op);
                    assert_eq!(*lhs, test_case.lhs);
                    assert_eq!(*rhs, test_case.rhs);
                } else {
                    panic!("Expected type ast::Expression::Infix");
                }
            } else {
                panic!("Expected type ast::Statement::Expression");
            }
        }
    }

    #[test]
    fn operator_precedence() {
        struct TestCase<'a> {
            input: &'a str,
            expected: &'a str,
            len: usize,
        }

        let test_cases = vec![
            TestCase {
                input: "-a * b",
                expected: "((-a) * b)",
                len: 1,
            },
            TestCase {
                input: "!-a",
                expected: "(!(-a))",
                len: 1,
            },
            TestCase {
                input: "a + b + c",
                expected: "((a + b) + c)",
                len: 1,
            },
            TestCase {
                input: "a * b * c",
                expected: "((a * b) * c)",
                len: 1,
            },
            TestCase {
                input: "a * b / c",
                expected: "((a * b) / c)",
                len: 1,
            },
            TestCase {
                input: "a + b / c",
                expected: "(a + (b / c))",
                len: 1,
            },
            TestCase {
                input: "a + b * c + d / e - f",
                expected: "(((a + (b * c)) + (d / e)) - f)",
                len: 1,
            },
            TestCase {
                input: "3 + 4; -5 * 5",
                expected: "(3 + 4)\n((-5) * 5)",
                len: 2,
            },
            TestCase {
                input: "5 > 4 == 3 < 4",
                expected: "((5 > 4) == (3 < 4))",
                len: 1,
            },
            TestCase {
                input: "5 < 4 != 3 > 4",
                expected: "((5 < 4) != (3 > 4))",
                len: 1,
            },
            TestCase {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
                len: 1,
            },
            TestCase {
                input: "true",
                expected: "true",
                len: 1,
            },
            TestCase {
                input: "false",
                expected: "false",
                len: 1,
            },
            TestCase {
                input: "3 > 5 == false",
                expected: "((3 > 5) == false)",
                len: 1,
            },
            TestCase {
                input: "3 < 5 == true",
                expected: "((3 < 5) == true)",
                len: 1,
            },
            TestCase {
                input: "1 + (2 + 3) + 4",
                expected: "((1 + (2 + 3)) + 4)",
                len: 1,
            },
            TestCase {
                input: "(5 + 5) * 2",
                expected: "((5 + 5) * 2)",
                len: 1,
            },
            TestCase {
                input: "2 / (5 + 5)",
                expected: "(2 / (5 + 5))",
                len: 1,
            },
            TestCase {
                input: "-(5 + 5)",
                expected: "(-(5 + 5))",
                len: 1,
            },
            TestCase {
                input: "!(true == true)",
                expected: "(!(true == true))",
                len: 1,
            },
        ];

        for test_case in test_cases {
            let mut parser = make_parser(test_case.input);
            let program = parser.parse_program();
            check_parser(&parser);

            assert_eq!(program.statements.len(), test_case.len);
            assert_eq!(program.to_string(), test_case.expected);
        }
    }

    #[test]
    fn if_expression() {
        let mut parser = make_parser("if (x < y) { x }");

        use ast::Expression::{Identifier, Infix};
        use ast::InfixOperator::LessThan;
        use ast::Statement::{Block, Expression};

        let expected_cond = Infix(
            Box::new(Identifier("x".to_owned())),
            LessThan,
            Box::new(Identifier("y".to_owned())),
        );

        let expected_if_true = Block(vec![Box::new(Expression(Identifier("x".to_owned())))]);

        let mut program = parser.parse_program();
        check_parser(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = program.statements.pop().unwrap();

        if let ast::Statement::Expression(expression) = statement {
            if let ast::Expression::If(cond, if_true) = expression {
                assert_eq!(*cond, expected_cond);
                assert_eq!(*if_true, expected_if_true);
            } else {
                panic!("Expected If expression");
            }
        } else {
            panic!("Expected ast::Statement::Expression");
        }
    }

    #[test]
    fn if_else_expression() {
        let mut parser = make_parser("if (x < y) { x } else { y }");

        use ast::Expression::{Identifier, Infix};
        use ast::InfixOperator::LessThan;
        use ast::Statement::{Block, Expression};

        let expected_cond = Infix(
            Box::new(Identifier("x".to_owned())),
            LessThan,
            Box::new(Identifier("y".to_owned())),
        );

        let expected_if_true = Block(vec![Box::new(Expression(Identifier("x".to_owned())))]);

        let expected_if_false = Block(vec![Box::new(Expression(Identifier("y".to_owned())))]);

        let mut program = parser.parse_program();
        check_parser(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = program.statements.pop().unwrap();

        if let ast::Statement::Expression(expression) = statement {
            if let ast::Expression::IfElse(cond, if_true, if_false) = expression {
                assert_eq!(*cond, expected_cond);
                assert_eq!(*if_true, expected_if_true);
                assert_eq!(*if_false, expected_if_false);
            } else {
                panic!("Expected If expression");
            }
        } else {
            panic!("Expected ast::Statement::Expression");
        }
    }

    fn make_parser(input: &str) -> Parser {
        let lexer = Lexer::new(input.chars().collect());
        Parser::new(lexer)
    }

    fn check_parser(parser: &Parser) {
        if parser.errors.is_empty() {
            return;
        }

        for msg in &parser.errors {
            println!("{}", msg);
        }

        panic!("parser had errors");
    }
}
