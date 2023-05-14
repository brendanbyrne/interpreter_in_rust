pub mod lexer;
use lexer::{Lexer, Token};

pub mod ast;

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

#[derive(PartialEq, PartialOrd)]
enum Ordering {
    Lowest,
    Equals,      // == or !=
    LessGreater, // > or <
    PlusMinus,   // + or -
    StarSlash,   // * or /
    Prefix,      // -x or !x
    Call,        // x()
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

    pub fn parse_program(&mut self) -> Option<ast::Program> {
        let mut program = ast::Program::new();

        while self.cur_token != Token::EOF {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        }

        Some(program)
    }

    fn next_token(&mut self) {
        // This seems weird to me.  Why do I have to clone here?
        // I want to do like a simultaneous exchange like this:
        //          next_token() -> peek -> cur
        //   but all at once
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Option<ast::StatementType> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::StatementType> {
        self.next_token(); // drop Let

        let name = if let Token::Ident(name) = self.cur_token.clone() {
            name
        } else {
            let msg = Parser::make_error(&Token::Ident("<name>".to_string()), &self.cur_token);
            self.errors.push(msg);
            return None;
        };

        if !self.expect_peek(&Token::Assign) {
            return None;
        }

        // TODO: Skipping to semicolon
        while self.cur_token != Token::Semicolon {
            self.next_token();
        }

        Some(ast::StatementType::Let(name, ast::ExpressionType::Empty))
    }

    fn parse_return_statement(&mut self) -> Option<ast::StatementType> {
        self.next_token(); // drop Return

        // TODO: Skipping to semicolon
        while self.cur_token != Token::Semicolon {
            self.next_token();
        }
        Some(ast::StatementType::Return(ast::ExpressionType::Empty))
    }

    fn parse_expression_statement(&mut self) -> Option<ast::StatementType> {
        let expression = self.parse_expression(Ordering::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Some(ast::StatementType::Expression(expression))
    }

    fn parse_expression(&mut self, ordering: Ordering) -> Option<ast::ExpressionType> {
        let mut expression = self.parse_prefix()?;

        while self.peek_token != Token::Semicolon && ordering < Parser::precedence(&self.peek_token)
        {
            if let Some(op) = Parser::get_infix_operator(&self.peek_token) {
                self.next_token(); // Move to infix operator
                expression = self.parse_infix_expression(expression, op)?;
            } else {
                return Some(expression);
            }
        }

        return Some(expression);
    }

    fn parse_prefix(&mut self) -> Option<ast::ExpressionType> {
        match &self.cur_token {
            Token::Ident(name) => return Some(ast::ExpressionType::Identifier(name.clone())),
            Token::Int(value) => return self.parse_prefix_int(value.to_string()),
            Token::Bang => return self.parse_prefix_expression(ast::PrefixOperator::Not),
            Token::Minus => return self.parse_prefix_expression(ast::PrefixOperator::Negate),
            Token::True => return Some(ast::ExpressionType::Boolean(true)),
            Token::False => return Some(ast::ExpressionType::Boolean(false)),
            _ => return None,
        };
    }

    fn parse_prefix_int(&mut self, input: String) -> Option<ast::ExpressionType> {
        if let Ok(value) = input.parse::<i128>() {
            return Some(ast::ExpressionType::Int(value));
        } else {
            self.errors
                .push(format!("Could not parse '{}' as an integer", input));
            return None;
        }
    }

    fn parse_prefix_expression(&mut self, op: ast::PrefixOperator) -> Option<ast::ExpressionType> {
        self.next_token(); // drop operator

        let expression = self.parse_expression(Ordering::Prefix)?;
        Some(ast::ExpressionType::Prefix(op, Box::new(expression)))
    }

    fn parse_infix_expression(
        &mut self,
        lhs: ast::ExpressionType,
        op: ast::InfixOperator,
    ) -> Option<ast::ExpressionType> {
        let precedence = Parser::precedence(&self.cur_token);
        self.next_token(); // Drops operator
        let rhs = self.parse_expression(precedence)?;
        Some(ast::ExpressionType::Infix(Box::new(lhs), op, Box::new(rhs)))
    }

    fn expect_peek(&mut self, token: &Token) -> bool {
        if &self.peek_token == token {
            self.next_token();
            return true;
        } else {
            let msg = Parser::make_error(token, &self.peek_token);
            self.errors.push(msg);
            return false;
        }
    }

    fn make_error(expected: &Token, found: &Token) -> String {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            expected, found
        );
        msg
    }

    fn get_infix_operator(token: &Token) -> Option<ast::InfixOperator> {
        match token {
            Token::Equal => return Some(ast::InfixOperator::Equal),
            Token::NotEqual => return Some(ast::InfixOperator::NotEqual),
            Token::LessThan => return Some(ast::InfixOperator::LessThan),
            Token::GreaterThan => return Some(ast::InfixOperator::GreaterThan),
            Token::Plus => return Some(ast::InfixOperator::Plus),
            Token::Minus => return Some(ast::InfixOperator::Minus),
            Token::Asterisk => return Some(ast::InfixOperator::Star),
            Token::Slash => return Some(ast::InfixOperator::Slash),
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
            Token::Asterisk => Ordering::StarSlash,
            Token::Slash => Ordering::StarSlash,
            _ => Ordering::Lowest,
        };
        p
    }
}

// TODO: Should this go into a separate file?
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
        let program = parser.parse_program().unwrap();
        check_parser(&parser);

        assert_eq!(program.statements.len(), 3);

        let tests = vec!["x", "y", "foobar"];

        for (s, t) in program.statements.iter().zip(tests.iter()) {
            if let ast::StatementType::Let(name, _) = s {
                assert_eq!(name, t);
            } else {
                panic!("Expected type ast::StatementType::Let");
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
        let program = parser.parse_program().unwrap();
        check_parser(&parser);

        assert_eq!(program.statements.len(), 3);

        for s in program.statements {
            if let ast::StatementType::Return(_) = s {
            } else {
                panic!("Expected type ast::StatementType::Return");
            }
        }
    }

    #[test]
    fn identifier_expression() {
        let mut parser = make_parser("foobar;");

        let mut program = parser.parse_program().unwrap();
        check_parser(&parser);

        assert_eq!(program.statements.len(), 1);

        if let ast::StatementType::Expression(expression) = program.statements.pop().unwrap() {
            if let ast::ExpressionType::Identifier(name) = expression {
                assert_eq!("foobar", name);
            } else {
                panic!("Expected type ast::ExpressionType::Indentifier");
            }
        } else {
            panic!("Expected type ast::StatementType::Expression");
        }
    }

    #[test]
    fn integer_literal_expression() {
        let mut parser = make_parser("5;");

        let mut program = parser.parse_program().unwrap();
        check_parser(&parser);

        assert_eq!(program.statements.len(), 1);

        if let ast::StatementType::Expression(expression) = program.statements.pop().unwrap() {
            if let ast::ExpressionType::Int(value) = expression {
                assert_eq!(5, value);
            } else {
                panic!("Expected type ast::ExpressionType::Indentifier");
            }
        } else {
            panic!("Expected type ast::StatementType::Expression");
        }
    }

    #[test]
    fn prefix_expression() {
        struct TestCase<'a> {
            input: &'a str,
            op: ast::PrefixOperator,
            value: i128,
        }

        let test_cases = vec![
            TestCase {
                input: "!5;",
                op: ast::PrefixOperator::Not,
                value: 5,
            },
            TestCase {
                input: "-15;",
                op: ast::PrefixOperator::Negate,
                value: 15,
            },
        ];

        for test_case in test_cases {
            let mut parser = make_parser(test_case.input);
            let mut program = parser.parse_program().unwrap();
            check_parser(&parser);

            assert_eq!(program.statements.len(), 1);

            // TODO: Is there a way to do this without nesting `if let`s like this?
            if let ast::StatementType::Expression(expression) = program.statements.pop().unwrap() {
                if let ast::ExpressionType::Prefix(op, nested_expression) = expression {
                    assert_eq!(op, test_case.op);

                    if let ast::ExpressionType::Int(value) = *nested_expression {
                        assert_eq!(value, test_case.value);
                    } else {
                        panic!("Expected type ast::ExpressionType::Int");
                    }
                } else {
                    panic!("Expected type ast::ExpressionType::Prefix");
                }
            } else {
                panic!("Expected type ast::StatementType::Expression");
            }
        }
    }

    #[test]
    fn infix_expression() {
        struct TestCase<'a> {
            input: &'a str,
            lhs: i128,
            rhs: i128,
            op: ast::InfixOperator,
        }

        let test_cases = vec![
            TestCase {
                input: "5 + 5",
                lhs: 5,
                rhs: 5,
                op: ast::InfixOperator::Plus,
            },
            TestCase {
                input: "5 - 5",
                lhs: 5,
                rhs: 5,
                op: ast::InfixOperator::Minus,
            },
            TestCase {
                input: "5 * 5",
                lhs: 5,
                rhs: 5,
                op: ast::InfixOperator::Star,
            },
            TestCase {
                input: "5 / 5",
                lhs: 5,
                rhs: 5,
                op: ast::InfixOperator::Slash,
            },
            TestCase {
                input: "5 > 5",
                lhs: 5,
                rhs: 5,
                op: ast::InfixOperator::GreaterThan,
            },
            TestCase {
                input: "5 < 5",
                lhs: 5,
                rhs: 5,
                op: ast::InfixOperator::LessThan,
            },
            TestCase {
                input: "5 == 5",
                lhs: 5,
                rhs: 5,
                op: ast::InfixOperator::Equal,
            },
            TestCase {
                input: "5 != 5",
                lhs: 5,
                rhs: 5,
                op: ast::InfixOperator::NotEqual,
            },
        ];

        for test_case in test_cases {
            let mut parser = make_parser(test_case.input);
            let mut program = parser.parse_program().unwrap();
            check_parser(&parser);

            assert_eq!(program.statements.len(), 1);

            // TODO: Is there a way to do this without nesting `if let`s like this?
            if let ast::StatementType::Expression(expression) = program.statements.pop().unwrap() {
                if let ast::ExpressionType::Infix(lhs, op, rhs) = expression {
                    assert_eq!(op, test_case.op);

                    if let ast::ExpressionType::Int(value) = *lhs {
                        assert_eq!(value, test_case.lhs);
                    } else {
                        panic!("Expected type ast::ExpressionType::Int");
                    }

                    if let ast::ExpressionType::Int(value) = *rhs {
                        assert_eq!(value, test_case.rhs);
                    } else {
                        panic!("Expected type ast::ExpressionType::Int");
                    }
                } else {
                    panic!("Expected type ast::ExpressionType::Infix");
                }
            } else {
                panic!("Expected type ast::StatementType::Expression");
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
                expected: "((-a) * b);",
                len: 1,
            },
            TestCase {
                input: "!-a",
                expected: "(!(-a));",
                len: 1,
            },
            TestCase {
                input: "a + b + c",
                expected: "((a + b) + c);",
                len: 1,
            },
            TestCase {
                input: "a * b * c",
                expected: "((a * b) * c);",
                len: 1,
            },
            TestCase {
                input: "a * b / c",
                expected: "((a * b) / c);",
                len: 1,
            },
            TestCase {
                input: "a + b / c",
                expected: "(a + (b / c));",
                len: 1,
            },
            TestCase {
                input: "a + b * c + d / e - f",
                expected: "(((a + (b * c)) + (d / e)) - f);",
                len: 1,
            },
            TestCase {
                input: "3 + 4; -5 * 5",
                expected: "(3 + 4);\n((-5) * 5);",
                len: 2,
            },
            TestCase {
                input: "5 > 4 == 3 < 4",
                expected: "((5 > 4) == (3 < 4));",
                len: 1,
            },
            TestCase {
                input: "5 < 4 != 3 > 4",
                expected: "((5 < 4) != (3 > 4));",
                len: 1,
            },
            TestCase {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
                len: 1,
            },
            TestCase {
                input: "true",
                expected: "true;",
                len: 1,
            },
            TestCase {
                input: "false",
                expected: "false;",
                len: 1,
            },
            TestCase {
                input: "3 > 5 == false",
                expected: "((3 > 5) == false);",
                len: 1,
            },
            TestCase {
                input: "3 < 5 == true",
                expected: "((3 < 5) == true);",
                len: 1,
            },
        ];

        for test_case in test_cases {
            let mut parser = make_parser(test_case.input);
            let program = parser.parse_program().unwrap();
            check_parser(&parser);

            assert_eq!(program.statements.len(), test_case.len);
            assert_eq!(program.to_string(), test_case.expected);
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
