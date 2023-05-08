pub mod lexer;
use lexer::{Lexer, Token};

pub mod ast;

pub struct Parser {
    lexer: Lexer,

    cur_token: Token,
    peek_token: Token,

    errors: Vec<String>,
}

enum Ordering {
    Lowest = 0,
    Equals = 1,       // ==
    LessGreater = 2,  // > or <
    Sum = 3,          // +
    Product = 4,      // *
    Prefix = 5,       // -x or !x
    Call = 6,         // x()
}

trait InfixParse {
    fn infix_parse(&self, expression: ast::ExpressionType) -> ast::ExpressionType;
}

impl InfixParse for Token {
    fn infix_parse(&self, expression: ast::ExpressionType) -> ast::ExpressionType{
	match self { 
	    _ => { return ast::ExpressionType::Empty; },
	};
    }
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
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
	    Token::LET => self.parse_let_statement(),
	    Token::RETURN => self.parse_return_statement(),
	    _ => self.parse_expression_statement(),
	}
    }

    fn parse_let_statement(&mut self) -> Option<ast::StatementType> {
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

	Some(ast::StatementType::Let{name, expression: ast::ExpressionType::Empty})
    }

    fn parse_return_statement(&mut self) -> Option<ast::StatementType> {
	self.next_token(); // drop RETURN
	
	// TODO: Skipping to semicolon
	while self.cur_token != Token::SEMICOLON {
	    self.next_token();
	}
	Some(ast::StatementType::Return(ast::ExpressionType::Empty))
    }

    fn parse_expression_statement(&mut self) -> Option<ast::StatementType> {
	let expression = self.parse_expression(Ordering::Lowest)?;

	if self.peek_token == Token::SEMICOLON {
	    self.next_token();
	}

	Some(ast::StatementType::Expression(expression))
    }

    fn parse_expression(&mut self, ordering: Ordering) -> Option<ast::ExpressionType> {
	let left_exp = self.prefix_parse()?;
	Some(left_exp)
    }

    fn prefix_parse(&mut self) -> Option<ast::ExpressionType> {
	match &self.cur_token {
	    Token::IDENT(name) => { return Some(ast::ExpressionType::Identifier(name.clone())); },
	    Token::INT(str_value) => {
		if let Ok(value) = str_value.parse::<i128>() {
		    return Some(ast::ExpressionType::Int(value));
		} else {
		    self.errors.push(format!("Could not parse {} as an integer", str_value));
		    return None;
		}
	    },
	    _ => { return None; },
	};
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

// TODO: Should this go into a separate file?
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn let_statements() {
	let mut parser = make_parser("
let x = 5;
let y = 10;
let foobar = 838383;
");	
	let program = parser.parse_program().unwrap();
	check_parser(&parser);
	
	assert_eq!(program.statements.len(), 3);

	let tests = vec!["x", "y", "foobar"];

	for (s, t) in program.statements.iter().zip(tests.iter()) {
	    if let ast::StatementType::Let{name, expression:_} = s {
		assert_eq!(name, t);
	    } else {
		panic!("Expected type ast::StatementType::Let");
	    }
	}
    }
    
    #[test]
    fn return_statements() {
	let mut parser = make_parser("
return 5;
return 10;
return 993322;
");
	let program = parser.parse_program().unwrap();
	check_parser(&parser);
	
	assert_eq!(program.statements.len(), 3);
	
	for s in program.statements {
	    if let ast::StatementType::Return(_) = s {
	    } else { panic!("Expected type ast::StatementType::Return"); }
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
	    }
	    else { panic!("Expected type ast::ExpressionType::Indentifier"); }
	}
	else { panic!("Expected type ast::StatementType::Expression"); }
    
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
	    }
	    else { panic!("Expected type ast::ExpressionType::Indentifier"); }
	}
	else { panic!("Expected type ast::StatementType::Expression"); }
    }

    #[test]
    fn prefix_expression() {
	struct TestCase<'a> {
	    input: &'a str,
	    operator: ast::PrefixOperator,
	    value: i128,
	}

	let test_cases = vec![TestCase{input: "!5;", operator: ast::PrefixOperator::Not, value: 5},
			      TestCase{input: "-15;", operator: ast::PrefixOperator::Negate, value: 15}];

	for test_case in test_cases {
	    let mut parser = make_parser(test_case.input);
	    let mut program = parser.parse_program().unwrap();
	    check_parser(&parser);

	    assert_eq!(program.statements.len(), 1);

	    // TODO: Is there a way to do this without nexting `if let`s like this?
	    if let ast::StatementType::Expression(expression) = program.statements.pop().unwrap() {
		
		if let ast::ExpressionType::Prefix(operator, nested_expression) = expression {
		    assert_eq!(operator, test_case.operator);
		    
		    if let ast::ExpressionType::Int(value) = *nested_expression {
			assert_eq!(value, test_case.value);

		    } else { panic!("Expected type ast::ExpressionType::Int"); }
		} else { panic!("Expected type ast::ExpressionType::Prefix"); }
	    } else { panic!("Expected type ast::StatementType::Expression"); }
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
