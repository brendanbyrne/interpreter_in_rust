mod token;

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
    fn new(input: Vec<char>) -> Self {
	let mut lexer = Self{input, position: 0, read_position: 0, ch: NUL};
	lexer.read_char();
	lexer
    }

    fn read_identifier(&mut self) -> Vec<char> {
	let position = self.position;
	while is_letter(self.ch) {
	    self.read_char();
	}
	self.input[position..self.position].to_vec()
    }

    fn read_int(&mut self) -> Vec<char> {
	let position = self.position;
	while is_digit(self.ch) {
	    self.read_char();
	}
	self.input[position..self.position].to_vec()
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

    pub fn next_token(&mut self) -> token::Token {
	let tok: token::Token;

	self.skip_whitespace();
	
	match self.ch {
	    '=' => {
		if self.peek_char() == '=' {
		    self.read_char();
		    tok = token::Token::EQ;
		} else {
		    tok = token::Token::ASSIGN;
		}
	    }
	    ';' => {
		tok = token::Token::SEMICOLON;
	    }
	    '(' => {
		tok = token::Token::LPAREN;
	    }
	    ')' => {
		tok = token::Token::RPAREN;
	    }
	    ',' => {
		tok = token::Token::COMMA;
	    }
	    '+' => {
		tok = token::Token::PLUS;
	    }
	    '{' => {
		tok = token::Token::LBRACE;
	    }
	    '}' => {
		tok = token::Token::RBRACE;
	    }
	    '-' => {
		tok = token::Token::MINUS;
	    }
	    '!' => {
		if self.peek_char() == '=' {
		    self.read_char();
		    tok = token::Token::NOT_EQ;
		} else {
		    tok = token::Token::BANG;
		}
	    }
	    '*' => {
		tok = token::Token::ASTERISK;
	    }
	    '/' => {
		tok = token::Token::SLASH;
	    }
	    '<' => {
		tok = token::Token::LT;
	    }
	    '>' => {
		tok = token::Token::GT;
	    }
	    NUL => {
		tok = token::Token::EOF;
	    }
	    _ => {
		if is_letter(self.ch) {
		    let literal = self.read_identifier();
		    return token::lookup_identifier(literal);		    
		} else if is_digit(self.ch) {
		    let literal = self.read_int();
		    return token::Token::INT(literal);
		} else {
		    tok = token::Token::ILLEGAL(self.ch);
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
	    IDENT(vec!['f', 'i', 'v', 'e']),
	    ASSIGN,
	    INT(vec!['5']),
	    SEMICOLON,
	    LET,
	    IDENT(vec!['t', 'e', 'n']),
	    ASSIGN,
	    INT(vec!['1', '0']),
	    SEMICOLON,
	    LET,
	    IDENT(vec!['a', 'd', 'd']),
	    ASSIGN,
	    FUNCTION,
	    LPAREN,
	    IDENT(vec!['x']),
	    COMMA,
	    IDENT(vec!['y']),
	    RPAREN,
	    LBRACE,
	    IDENT(vec!['x']),
	    PLUS,
	    IDENT(vec!['y']),
	    SEMICOLON,
	    RBRACE,
	    SEMICOLON,
	    LET,
	    IDENT(vec!['r', 'e', 's', 'u', 'l', 't']),
	    ASSIGN,
	    IDENT(vec!['a', 'd', 'd']),
	    LPAREN,
	    IDENT(vec!['f', 'i', 'v', 'e']),
	    COMMA,
	    IDENT(vec!['t', 'e', 'n']),
	    RPAREN,
	    SEMICOLON,
	    BANG,
	    MINUS,
	    SLASH,
	    ASTERISK,
	    INT(vec!['5']),
	    SEMICOLON,
	    INT(vec!['5']),
	    LT,
	    INT(vec!['1', '0']),
	    GT,
	    INT(vec!['5']),
	    SEMICOLON,
	    IF,
	    LPAREN,
	    INT(vec!['5']),
	    LT,
	    INT(vec!['1', '0']),
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
	    INT(vec!['1', '0']),
	    EQ,
	    INT(vec!['1', '0']),
	    SEMICOLON,
	    INT(vec!['1', '0']),
	    NOT_EQ,
	    INT(vec!['9']),
	    SEMICOLON,
	    EOF
	];
	    
		
	let mut lexer = Lexer::new(input.chars().collect());

	for expected_token in tokens {
	    assert_eq!(expected_token, lexer.next_token());
	}
    }
}
