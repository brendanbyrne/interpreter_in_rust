//! Implementation of the Monkey language parser

mod error;
use error::{Error, Result};

mod lexer;
use lexer::{Lexer, Token};

pub mod ast;
pub use ast::Program;

#[cfg(test)]
mod tests;

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
            cur_token: Token::Eof,
            peek_token: Token::Eof,
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

        while self.cur_token != Token::Eof {
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

        Ok(expression)
    }

    fn parse_prefix(&mut self) -> Result<ast::Expression> {
        match &self.cur_token {
            Token::Ident(name) => Ok(ast::Expression::Identifier(name.clone())),
            Token::Int(value) => self.parse_prefix_int(value.to_string()),
            Token::Bang => self.parse_prefix_expression(ast::PrefixOperator::Not),
            Token::Minus => self.parse_prefix_expression(ast::PrefixOperator::Negate),
            Token::True => Ok(ast::Expression::Bool(true)),
            Token::False => Ok(ast::Expression::Bool(false)),
            Token::LParen => self.parse_group(),
            Token::If => self.parse_if(),
            Token::Function => self.parse_function_literal(),
            _ => Err(Error::NoPrefixDefined(self.cur_token.clone())),
        }
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
        Ok(expression)
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

        Ok(ast::Expression::If(
            Box::new(condition),
            Box::new(consequence),
        ))
    }

    fn parse_block_statement(&mut self) -> Result<ast::Statement> {
        assert_eq!(self.cur_token, Token::LBrace);
        self.next_token();

        let mut statements = Vec::new();

        while self.cur_token != Token::RBrace && self.cur_token != Token::Eof {
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
            Token::Equal => Some(ast::InfixOperator::Equal),
            Token::NotEqual => Some(ast::InfixOperator::NotEqual),
            Token::LessThan => Some(ast::InfixOperator::LessThan),
            Token::GreaterThan => Some(ast::InfixOperator::GreaterThan),
            Token::Plus => Some(ast::InfixOperator::Plus),
            Token::Minus => Some(ast::InfixOperator::Minus),
            Token::Asterisk => Some(ast::InfixOperator::Multiply),
            Token::Slash => Some(ast::InfixOperator::Divide),
            Token::LParen => Some(ast::InfixOperator::Call),
            _ => None,
        }
    }

    fn precedence(token: &Token) -> Ordering {
        match token {
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
        }
    }
}
