#[derive(Debug)]
pub struct Token {
    type_: &'static str,
    literal: String,
}

impl Token {
    fn new() -> Token {
	Token{type_: ILLEGAL, literal: "Hello world".to_string()}
    }
}

//
// Token Types
//
macro_rules! DTT { // Declare a type of token
    ($name:ident, $value:expr) => {const $name: &str = $value;};
}

DTT!(ILLEGAL, "ILLEGAL");
DTT!(EOF, "EOF");

// Identifiers + literals
DTT!(IDENT, "IDENT"); // add, foobar, x, y, ...
DTT!(INT, "INT"); // 123456

// // Operators
DTT!(ASSIGN, "=");
DTT!(PLUS, "+");

// Delimiters
DTT!(COMMA, ",");
DTT!(SEMICOLON, ";");

DTT!(LPAREN, "(");
DTT!(RPAREN, ")");
DTT!(LBRACE, "{");
DTT!(RBRACE, "}");

// Keywords
DTT!(FUNCTION, "FUNCTION");
DTT!(LET, "LET");

#[cfg(test)]
mod tests{
    use super::*;
    
    #[test]
    fn build_token_with_const() {
	let token = Token::new();
	assert_eq!(token.type_, ILLEGAL);
	assert_eq!(token.literal, "Hello world");
    }
}
