mod token;

pub struct Lexer {
    input: Vec<char>,
    pub position: usize,
    pub read_position: usize,
    pub ch: char,
}

const NUL: char = '\u{0}';

impl Lexer {
    fn new(input: Vec<char>) -> Self {
	let mut lexer = Self{input, position: 0, read_position: 0, ch: NUL};
	lexer.read_char();
	lexer
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
		todo!()
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
	let input = "=+(){},;".to_string();

	let tokens = vec![token::Token::ASSIGN,
			  token::Token::PLUS,
			  token::Token::LPAREN,
			  token::Token::RPAREN,
			  token::Token::LBRACE,
			  token::Token::RBRACE,
			  token::Token::COMMA,
			  token::Token::SEMICOLON];
		
	let mut lexer = Lexer::new(input.chars().collect());

	for expected_token in tokens {
	    assert_eq!(expected_token, lexer.next_token());
	}
    }
}
