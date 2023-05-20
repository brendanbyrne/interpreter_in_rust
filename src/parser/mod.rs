//! Implementation of the Monkey language parser

mod error;
use error::{Error, Result};

mod lexer;
use lexer::{Lexer, Token};

pub mod ast;
pub use ast::Program;

/// Returns a valid program or the parser error message
///
/// * `input` - A string slice containing Monkey code
pub fn parse_program(input: &str) -> Result<Program> {
    let lexer = Lexer::new(input.chars().collect());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program()?;
    Ok(program)
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

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<Error>,
}

impl Parser {
    /// Create a Parser object
    ///
    /// * `lexer` - A Lexer containing the tokinized input
    fn new(lexer: Lexer) -> Self {
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

    /// Returns a program or the logged errors
    ///
    /// Parse inputs and log any errors encountered
    fn parse_program(&mut self) -> Result<Program> {
        let mut program = Program::new();

        while self.cur_token != Token::EOF {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(Box::new(statement)),
                Err(e) => self.errors.push(e),
            };
            self.next_token();
        }

        if !self.errors.is_empty() {
            return Err(Error::ParsingFailed(
                self.errors
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join("\n"),
            ));
        }

        Ok(program)
    }

    /// Advance the tokens within the lexer
    fn next_token(&mut self) {
        // QUESTION: This seems weird to me.  Why do I have to clone here?
        // I want to do like a simultaneous exchange like this:
        //          next_token() -> peek -> cur
        //   but all at once
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    /// Returns a statement or the reason why it couldn't be parsed
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
            Err(Error::PeekMismatch(
                Token::Ident("<name>".to_owned()),
                self.cur_token.clone(),
            ))
        }
    }

    fn parse_let_statement(&mut self) -> Result<ast::Statement> {
        assert_eq!(self.cur_token, Token::Let);
        self.next_token();

        let name = self.get_ident_name()?;

        self.expect_peek(&Token::Assign)?;
        self.next_token();

        let expression = self.parse_expression(Ordering::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(ast::Statement::Let(name, expression))
    }

    fn parse_return_statement(&mut self) -> Result<ast::Statement> {
        assert_eq!(self.cur_token, Token::Return);
        self.next_token();

        let expression = self.parse_expression(Ordering::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(ast::Statement::Return(expression))
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
                expression = self.parse_infix_expression(op, expression)?;
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
            Token::True => return Ok(ast::Expression::Bool(true)),
            Token::False => return Ok(ast::Expression::Bool(false)),
            Token::LParen => return self.parse_group(),
            Token::If => return self.parse_if(),
            Token::Function => return self.parse_function_literal(),
            _ => return Err(Error::NoPrefixDefined(self.cur_token.clone())),
        };
    }

    fn parse_prefix_int(&mut self, input: String) -> Result<ast::Expression> {
        if let Ok(value) = input.parse::<i128>() {
            Ok(ast::Expression::Int(value))
        } else {
            Err(Error::AsIntFailed(input))
        }
    }

    fn parse_prefix_expression(&mut self, op: ast::PrefixOperator) -> Result<ast::Expression> {
        self.next_token(); // drop operator

        let expression = self.parse_expression(Ordering::Prefix)?;
        Ok(ast::Expression::Prefix(op, Box::new(expression)))
    }

    fn parse_infix_expression(
        &mut self,
        op: ast::InfixOperator,
        lhs: ast::Expression,
    ) -> Result<ast::Expression> {
        let precedence = Parser::precedence(&self.cur_token);

        // Making a strong personal choice to treat Calls as a special case
        // rather than a proper InfixOperator.  Well see if this is a good
        // idea.
        if op == ast::InfixOperator::Call {
            let arguments = self.parse_call_args()?;
            Ok(ast::Expression::Call(Box::new(lhs), arguments))
        } else {
            self.next_token(); // drop operator
            let rhs = self.parse_expression(precedence)?;
            Ok(ast::Expression::Infix(op, Box::new(lhs), Box::new(rhs)))
        }
    }

    fn parse_call_args(&mut self) -> Result<Vec<Box<ast::Expression>>> {
        assert_eq!(self.cur_token, Token::LParen);
        self.next_token();

        if self.cur_token == Token::RParen {
            return Ok(vec![]);
        }

        let mut args = {
            let arg = self.parse_expression(Ordering::Lowest)?;
            vec![Box::new(arg)]
        };

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            let arg = self.parse_expression(Ordering::Lowest)?;
            args.push(Box::new(arg));
        }

        self.expect_peek(&Token::RParen)?;

        Ok(args)
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
                Err(e) => self.errors.push(e),
            };
            self.next_token();
        }

        Ok(ast::Statement::Block(statements))
    }

    fn parse_function_literal(&mut self) -> Result<ast::Expression> {
        assert_eq!(self.cur_token, Token::Function);

        self.expect_peek(&Token::LParen)?;

        let params = self.parse_func_params()?;

        self.expect_peek(&Token::LBrace)?;

        let body = self.parse_block_statement()?;

        Ok(ast::Expression::Function(params, Box::new(body)))
    }

    fn parse_func_params(&mut self) -> Result<Vec<Box<ast::Expression>>> {
        assert_eq!(self.cur_token, Token::LParen);
        self.next_token();

        if self.cur_token == Token::RParen {
            return Ok(vec![]);
        }

        let mut params = {
            let name = self.get_ident_name()?;
            vec![Box::new(ast::Expression::Identifier(name))]
        };

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            let name = self.get_ident_name()?;
            params.push(Box::new(ast::Expression::Identifier(name)));
        }

        self.expect_peek(&Token::RParen)?;

        Ok(params)
    }

    fn expect_peek(&mut self, token: &Token) -> Result<()> {
        if &self.peek_token == token {
            self.next_token();
            Ok(())
        } else {
            Err(Error::PeekMismatch(token.clone(), self.peek_token.clone()))
        }
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
            Token::LParen => return Some(ast::InfixOperator::Call),
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
            Token::LParen => Ordering::Call,
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
        struct TestCase<'a> {
            input: &'a str,
            expected_name: &'a str,
            expected_expression: ast::Expression,
        }

        let test_cases = vec![
            TestCase {
                input: "let x = 5;",
                expected_name: "x",
                expected_expression: ast::Expression::Int(5),
            },
            TestCase {
                input: "let y = 10;",
                expected_name: "y",
                expected_expression: ast::Expression::Int(10),
            },
            TestCase {
                input: "let foobar = 838383;",
                expected_name: "foobar",
                expected_expression: ast::Expression::Int(838383),
            },
        ];

        for test_case in test_cases {
            let mut program = parse_program(test_case.input).unwrap();
            assert_eq!(program.statements.len(), 1);

            if let ast::Statement::Let(name, expression) = *program.statements.pop().unwrap() {
                assert_eq!(name, test_case.expected_name);
                assert_eq!(expression, test_case.expected_expression);
            } else {
                panic!("Expected type ast::Statement::Let");
            }
        }
    }

    #[test]
    fn return_statements() {
        struct TestCase<'a> {
            input: &'a str,
            expected_expression: ast::Expression,
        }

        let test_cases = vec![
            TestCase {
                input: "return 5;",
                expected_expression: ast::Expression::Int(5),
            },
            TestCase {
                input: "return 10;",
                expected_expression: ast::Expression::Int(10),
            },
            TestCase {
                input: "return 993322",
                expected_expression: ast::Expression::Int(993322),
            },
        ];

        for test_case in test_cases {
            let mut program = parse_program(test_case.input).unwrap();
            assert_eq!(program.statements.len(), 1);

            if let ast::Statement::Return(expression) = *program.statements.pop().unwrap() {
                assert_eq!(expression, test_case.expected_expression);
            } else {
                panic!("Expected type ast::Statement::Return");
            }
        }
    }

    #[test]
    fn identifier_expression() {
        let mut program = parse_program("foobar").unwrap();
        assert_eq!(program.statements.len(), 1);

        if let ast::Expression::Identifier(name) = get_expression(&mut program) {
            assert_eq!("foobar", name);
        } else {
            panic!("Expected type ast::Expression::Indentifier");
        }
    }

    #[test]
    fn integer_literal_expression() {
        let mut program = parse_program("5").unwrap();
        assert_eq!(program.statements.len(), 1);

        if let ast::Expression::Int(value) = get_expression(&mut program) {
            assert_eq!(5, value);
        } else {
            panic!("Expected type ast::Expression::Indentifier");
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
                value: ast::Expression::Bool(true),
            },
            TestCase {
                input: "!false",
                op: ast::PrefixOperator::Not,
                value: ast::Expression::Bool(false),
            },
        ];

        for test_case in test_cases {
            let mut program = parse_program(test_case.input).unwrap();
            assert_eq!(program.statements.len(), 1);

            if let ast::Expression::Prefix(op, nested_expression) = get_expression(&mut program) {
                assert_eq!(op, test_case.op);
                assert_eq!(*nested_expression, test_case.value);
            } else {
                panic!("Expected type ast::Expression::Prefix");
            }
        }
    }

    #[test]
    fn infix_expression() {
        struct TestCase<'a> {
            input: &'a str,
            op: ast::InfixOperator,
            lhs: ast::Expression,
            rhs: ast::Expression,
        }

        let test_cases = vec![
            TestCase {
                input: "5 + 5",
                op: ast::InfixOperator::Plus,
                lhs: ast::Expression::Int(5),
                rhs: ast::Expression::Int(5),
            },
            TestCase {
                input: "5 - 5",
                op: ast::InfixOperator::Minus,
                lhs: ast::Expression::Int(5),
                rhs: ast::Expression::Int(5),
            },
            TestCase {
                input: "5 * 5",
                op: ast::InfixOperator::Multiply,
                lhs: ast::Expression::Int(5),
                rhs: ast::Expression::Int(5),
            },
            TestCase {
                input: "5 / 5",
                op: ast::InfixOperator::Divide,
                lhs: ast::Expression::Int(5),
                rhs: ast::Expression::Int(5),
            },
            TestCase {
                input: "5 > 5",
                op: ast::InfixOperator::GreaterThan,
                lhs: ast::Expression::Int(5),
                rhs: ast::Expression::Int(5),
            },
            TestCase {
                input: "5 < 5",
                op: ast::InfixOperator::LessThan,
                lhs: ast::Expression::Int(5),
                rhs: ast::Expression::Int(5),
            },
            TestCase {
                input: "5 == 5",
                op: ast::InfixOperator::Equal,
                lhs: ast::Expression::Int(5),
                rhs: ast::Expression::Int(5),
            },
            TestCase {
                input: "5 != 5",
                op: ast::InfixOperator::NotEqual,
                lhs: ast::Expression::Int(5),
                rhs: ast::Expression::Int(5),
            },
            TestCase {
                input: "true == true",
                op: ast::InfixOperator::Equal,
                lhs: ast::Expression::Bool(true),
                rhs: ast::Expression::Bool(true),
            },
            TestCase {
                input: "true != false",
                op: ast::InfixOperator::NotEqual,
                lhs: ast::Expression::Bool(true),
                rhs: ast::Expression::Bool(false),
            },
            TestCase {
                input: "false == false",
                op: ast::InfixOperator::Equal,
                lhs: ast::Expression::Bool(false),
                rhs: ast::Expression::Bool(false),
            },
        ];

        for test_case in test_cases {
            let mut program = parse_program(test_case.input).unwrap();
            assert_eq!(program.statements.len(), 1);

            if let ast::Expression::Infix(op, lhs, rhs) = get_expression(&mut program) {
                assert_eq!(op, test_case.op);
                assert_eq!(*lhs, test_case.lhs);
                assert_eq!(*rhs, test_case.rhs);
            } else {
                panic!("Expected type ast::Expression::Infix");
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
            TestCase {
                input: "1 + (2 + 3) + 4",
                expected: "((1 + (2 + 3)) + 4);",
                len: 1,
            },
            TestCase {
                input: "(5 + 5) * 2",
                expected: "((5 + 5) * 2);",
                len: 1,
            },
            TestCase {
                input: "2 / (5 + 5)",
                expected: "(2 / (5 + 5));",
                len: 1,
            },
            TestCase {
                input: "-(5 + 5)",
                expected: "(-(5 + 5));",
                len: 1,
            },
            TestCase {
                input: "!(true == true)",
                expected: "(!(true == true));",
                len: 1,
            },
            TestCase {
                input: "a + add(b * c) + d",
                expected: "((a + add((b * c))) + d);",
                len: 1,
            },
            TestCase {
                input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));",
                len: 1,
            },
            TestCase {
                input: "add(a + b + c * d / f + g)",
                expected: "add((((a + b) + ((c * d) / f)) + g));",
                len: 1,
            },
        ];

        for test_case in test_cases {
            let program = parse_program(test_case.input).unwrap();
            assert_eq!(program.statements.len(), test_case.len);
            assert_eq!(program.to_string(), test_case.expected);
        }
    }

    #[test]
    fn if_expression() {
        let mut program = parse_program("if (x < y) { x }").unwrap();

        use ast::Expression::{Identifier, Infix};
        use ast::InfixOperator::LessThan;
        use ast::Statement::{Block, Expression};

        let expected_cond = Infix(
            LessThan,
            Box::new(Identifier("x".to_owned())),
            Box::new(Identifier("y".to_owned())),
        );

        let expected_if_true = Block(vec![Box::new(Expression(Identifier("x".to_owned())))]);

        assert_eq!(program.statements.len(), 1);

        if let ast::Expression::If(cond, if_true) = get_expression(&mut program) {
            assert_eq!(*cond, expected_cond);
            assert_eq!(*if_true, expected_if_true);
        } else {
            panic!("Expected If expression");
        }
    }

    #[test]
    fn if_else_expression() {
        let mut program = parse_program("if (x < y) { x } else { y }").unwrap();

        use ast::Expression::{Identifier, Infix};
        use ast::InfixOperator::LessThan;
        use ast::Statement::{Block, Expression};

        let expected_cond = Infix(
            LessThan,
            Box::new(Identifier("x".to_owned())),
            Box::new(Identifier("y".to_owned())),
        );

        let expected_if_true = Block(vec![Box::new(Expression(Identifier("x".to_owned())))]);

        let expected_if_false = Block(vec![Box::new(Expression(Identifier("y".to_owned())))]);

        assert_eq!(program.statements.len(), 1);

        if let ast::Expression::IfElse(cond, if_true, if_false) = get_expression(&mut program) {
            assert_eq!(*cond, expected_cond);
            assert_eq!(*if_true, expected_if_true);
            assert_eq!(*if_false, expected_if_false);
        } else {
            panic!("Expected ast::Expression::If expression");
        }
    }

    #[test]
    fn function_literal_expression() {
        let mut program = parse_program("fn(x, y) { x + y; }").unwrap();
        assert_eq!(program.statements.len(), 1);

        use ast::Expression::{Identifier, Infix};
        use ast::InfixOperator::Plus;
        use ast::Statement::{Block, Expression};

        let x = Identifier("x".to_owned());
        let y = Identifier("y".to_owned());
        let expected_params = vec![Box::new(x.clone()), Box::new(y.clone())];
        let expected_body = Block(vec![Box::new(Expression(Infix(
            Plus,
            Box::new(x),
            Box::new(y),
        )))]);

        if let ast::Expression::Function(params, body) = get_expression(&mut program) {
            assert_eq!(params, expected_params);
            assert_eq!(*body, expected_body);
        } else {
            panic!("Expected ast::Expression::Function");
        }
    }

    #[test]
    fn function_parameters() {
        struct TestCase<'a> {
            input: &'a str,
            expected_params: Vec<Box<ast::Expression>>,
        }

        fn make_id(id: &str) -> Box<ast::Expression> {
            return Box::new(ast::Expression::Identifier(id.to_owned()));
        }

        let test_cases = vec![
            TestCase {
                input: "fn() {};",
                expected_params: vec![],
            },
            TestCase {
                input: "fn(x) {};",
                expected_params: vec![make_id("x")],
            },
            TestCase {
                input: "fn(x, y, z) {};",
                expected_params: vec![make_id("x"), make_id("y"), make_id("z")],
            },
        ];

        for test_case in test_cases {
            let mut program = parse_program(test_case.input).unwrap();

            if let ast::Expression::Function(params, _) = get_expression(&mut program) {
                assert_eq!(params, test_case.expected_params);
            } else {
                panic!("Expected ast::Expression::Function");
            }
        }
    }

    #[test]
    fn call_expression() {
        let mut program = parse_program("add(1, 2 * 3, 4 + 5);").unwrap();
        assert_eq!(program.statements.len(), 1);

        use ast::Expression::{Identifier, Infix, Int};
        use ast::InfixOperator::{Multiply, Plus};

        let expected_name = Identifier("add".to_owned());
        let expected_inputs = vec![
            Box::new(Int(1)),
            Box::new(Infix(Multiply, Box::new(Int(2)), Box::new(Int(3)))),
            Box::new(Infix(Plus, Box::new(Int(4)), Box::new(Int(5)))),
        ];

        if let ast::Expression::Call(name, inputs) = get_expression(&mut program) {
            assert_eq!(*name, expected_name);
            assert_eq!(inputs, expected_inputs);
        } else {
            panic!("Expected ast::Expression::Call");
        }
    }

    fn get_expression(program: &mut Program) -> ast::Expression {
        if let ast::Statement::Expression(expression) = *program.statements.pop().unwrap() {
            return expression;
        } else {
            panic!("Expected ast::Statement::Expression");
        }
    }
}
