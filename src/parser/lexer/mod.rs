mod token;
pub use token::Token;

pub struct Lexer {
    input: Vec<char>,
    pub position: usize,
    pub read_position: usize,
    pub ch: char,
}

const NUL: char = '\u{0}';

fn is_letter(ch: char) -> bool {
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

fn is_whitespace(ch: char) -> bool {
    ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
}

impl Lexer {
    pub fn new(input: Vec<char>) -> Self {
	let mut lexer = Self{input, position: 0, read_position: 0, ch: NUL};
	lexer.read_char();
	lexer
    }

    fn read_identifier(&mut self) -> String {
	let position = self.position;
	while is_letter(self.ch) {
	    self.read_char();
	}
	self.input[position..self.position].to_vec().iter().collect()
    }

    fn read_int(&mut self) -> String {
	let position = self.position;
	while is_digit(self.ch) {
	    self.read_char();
	}
	self.input[position..self.position].to_vec().iter().collect()
    }

    fn skip_whitespace(&mut self) {
	while is_whitespace(self.ch) {
	    self.read_char();
	}
    }

    fn peek_char(&self) -> char {
	if self.read_position >= self.input.len() {
	    return NUL;
	}
	return self.input[self.read_position];
    }
    
    fn read_char(&mut self) {
	if self.read_position >= self.input.len() {
	    self.ch = NUL;
	} else {
	    self.ch = self.input[self.read_position];
	}
	self.position = self.read_position;
	self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
	let tok: Token;

	self.skip_whitespace();
	
	match self.ch {
	    '=' => {
		if self.peek_char() == '=' {
		    self.read_char();
		    tok = Token::EQ;
		} else {
		    tok = Token::ASSIGN;
		}
	    }
	    ';' => {
		tok = Token::SEMICOLON;
	    }
	    '(' => {
		tok = Token::LPAREN;
	    }
	    ')' => {
		tok = Token::RPAREN;
	    }
	    ',' => {
		tok = Token::COMMA;
	    }
	    '+' => {
		tok = Token::PLUS;
	    }
	    '{' => {
		tok = Token::LBRACE;
	    }
	    '}' => {
		tok = Token::RBRACE;
	    }
	    '-' => {
		tok = Token::MINUS;
	    }
	    '!' => {
		if self.peek_char() == '=' {
		    self.read_char();
		    tok = Token::NOT_EQ;
		} else {
		    tok = Token::BANG;
		}
	    }
	    '*' => {
		tok = Token::ASTERISK;
	    }
	    '/' => {
		tok = Token::SLASH;
	    }
	    '<' => {
		tok = Token::LT;
	    }
	    '>' => {
		tok = Token::GT;
	    }
	    NUL => {
		tok = Token::EOF;
	    }
	    _ => {
		if is_letter(self.ch) {
		    let literal = self.read_identifier();
		    return token::lookup_identifier(literal);		    
		} else if is_digit(self.ch) {
		    let literal = self.read_int();
		    return Token::INT(literal);
		} else {
		    tok = Token::ILLEGAL(self.ch);
		}
	    }
	}

	self.read_char();
	tok
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_next_token() {
	let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
  return false;
}

10 == 10;
10 != 9;
".to_string();

	use token::Token::*;
	let tokens = vec![
	    LET,
	    IDENT("five".to_string()),
	    ASSIGN,
	    INT("5".to_string()),
	    SEMICOLON,
	    LET,
	    IDENT("ten".to_string()),
	    ASSIGN,
	    INT("10".to_string()),
	    SEMICOLON,
	    LET,
	    IDENT("add".to_string()),
	    ASSIGN,
	    FUNCTION,
	    LPAREN,
	    IDENT("x".to_string()),
	    COMMA,
	    IDENT("y".to_string()),
	    RPAREN,
	    LBRACE,
	    IDENT("x".to_string()),
	    PLUS,
	    IDENT("y".to_string()),
	    SEMICOLON,
	    RBRACE,
	    SEMICOLON,
	    LET,
	    IDENT("result".to_string()),
	    ASSIGN,
	    IDENT("add".to_string()),
	    LPAREN,
	    IDENT("five".to_string()),
	    COMMA,
	    IDENT("ten".to_string()),
	    RPAREN,
	    SEMICOLON,
	    BANG,
	    MINUS,
	    SLASH,
	    ASTERISK,
	    INT("5".to_string()),
	    SEMICOLON,
	    INT("5".to_string()),
	    LT,
	    INT("10".to_string()),
	    GT,
	    INT("5".to_string()),
	    SEMICOLON,
	    IF,
	    LPAREN,
	    INT("5".to_string()),
	    LT,
	    INT("10".to_string()),
	    RPAREN,
	    LBRACE,
	    RETURN,
	    TRUE,
	    SEMICOLON,
	    RBRACE,
	    ELSE,
	    LBRACE,
	    RETURN,
	    FALSE,
	    SEMICOLON,
	    RBRACE,
	    INT("10".to_string()),
	    EQ,
	    INT("10".to_string()),
	    SEMICOLON,
	    INT("10".to_string()),
	    NOT_EQ,
	    INT("9".to_string()),
	    SEMICOLON,
	    EOF
	];
	
	let mut lexer = Lexer::new(input.chars().collect());

	for expected_token in tokens {
	    assert_eq!(expected_token, lexer.next_token());
	}
    }
}
