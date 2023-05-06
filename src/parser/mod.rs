pub mod lexer;
use lexer::{Lexer, Token};

mod ast;

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
	let mut parser =
	    Parser{
		lexer,
		cur_token: Token::EOF,
		peek_token: Token::EOF,
		errors: Vec::new(),
	    };
	parser.next_token();
	parser.next_token();
	parser
    }

    fn next_token(&mut self) {
	// This seems weird to me.  Why do I have to clone here?
	// I want to do like a simultaneous exchange like this:
	//          next_token() -> peek -> cur
	//   but all at once
	self.cur_token = self.peek_token.clone();
	self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Option<ast::Program> {
	let mut program = ast::Program::new();

	while self.cur_token != Token::EOF {
	    if let Some(statement) = self.parse_statement() {
		program.statements.push(statement);
	    }
	    self.next_token();
	}
	
	Some(program)
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
	match self.cur_token {
	    Token::LET => self.parse_let_statement(),
	    _ => None,
	}
    }

    fn parse_let_statement(&mut self) -> Option<ast::Statement> {
	self.next_token(); // drop LET
	
	let name = if let Token::IDENT(name) = self.cur_token.clone() {
	    name
	} else {
	    let msg = Parser::make_error(&Token::IDENT("<name>".to_string()), &self.cur_token);
	    self.errors.push(msg);
	    return None;
	};
	
	if !self.expect_peek(&Token::ASSIGN) {
	    return None;
	}

	// TODO: Skipping to semicolon
	while self.cur_token != Token::SEMICOLON {
	    self.next_token();
	}

	Some(ast::Statement::Let{name, expression: ast::Expression::None})
    }

    fn make_error(expected: &Token, found: &Token) -> String {
	let msg = format!(
	    "expected next token to be {:?}, got {:?} instead",
	    expected,
	    found
	);
	msg
    }
    
    fn expect_peek(&mut self, token: &Token) -> bool{
	if &self.peek_token == token {
	    self.next_token();
	    return true;
	} else {
	    let msg = Parser::make_error(token, &self.peek_token);
	    self.errors.push(msg);
	    return false;
	}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parser() {
	let input = "
let x 5;
let = 10;
let 838383;
".to_string();

	let lexer = Lexer::new(input.chars().collect());
	let mut parser = Parser::new(lexer);
	
	let program = parser.parse_program().unwrap();
	check_parser(&parser);
	
	assert_eq!(program.statements.len(), 3);

	let tests = vec!["x", "y", "foobar"];

	for (s, t) in program.statements.iter().zip(tests.iter()) {
	    test_let_statement(s, t.to_string());
	}
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
    
    fn test_let_statement(s: &ast::Statement, t: String) {
	if let ast::Statement::Let{name, expression:_} = s {
	    assert_eq!(name, &t);
	} else {
	    panic!("Should always be type ast::Statement::Let");
	}
    }
}
