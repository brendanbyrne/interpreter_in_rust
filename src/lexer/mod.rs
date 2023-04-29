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
    
    pub fn read_char(&mut self) {
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
	match self.ch {
	    '=' => {
		tok = token::Token::ASSIGN;
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
	    NUL => {
		tok = token::Token::EOF;
	    }
	    _ => {
		if is_letter(self.ch) {
		    tok = token::Token::IDENT(self.read_identifier());
		    return tok;
		} else {
		    tok = token::Token::ILLEGAL;
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
let add = fn(x,y) {
  x + y;
};

let result = add(five, ten);
".to_string();

	use token::*;
	let tokens = vec![Token::LET];
	    
		
	let mut lexer = Lexer::new(input.chars().collect());

	for expected_token in tokens {
	    assert_eq!(expected_token, lexer.next_token());
	}
    }
}
