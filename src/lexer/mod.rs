mod token;

struct Lexer {
    input: String,
    position: u32, // current position in input (points to current char)
    read_position: u32, // current reading position in input (after current char)
    ch: char, // current char under examination
}

impl Lexer {
    fn new(input: String) -> Lexer {
	Lexer{input, position: 0, read_position: 0, ch: ' '}
    }

    fn next_token(&self) -> token::Token {
	token::Token::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_next_token() {
	let input = "=+(){},;".to_string();

	let tests = vec![(token::ASSIGN, "=".to_string())];
	
	let lexer = Lexer::new(input);

	for (expected_token, expected_literal) in tests.iter() {
	    let t = lexer.next_token();
	    assert_eq!(&t.type_, expected_token);
	    assert_eq!(&t.literal, expected_literal);
	}
    }
}
    
