#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Token {
    ILLEGAL(char),
    EOF,
    IDENT(String),
    INT(String),
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    #[allow(non_camel_case_types)]
    NOT_EQ,
}

fn keyword_check(literal: &String) -> Option<Token> {
    match literal.as_str() {
	"let" => Some(Token::LET),
	"fn" => Some(Token::FUNCTION),
	"true" => Some(Token::TRUE),
	"false" => Some(Token::FALSE),
	"if" => Some(Token::IF),
	"else" => Some(Token::ELSE),
	"return" => Some(Token::RETURN),
	_ => None,
    }
}

pub fn lookup_identifier(literal: String) -> Token {
    match keyword_check(&literal) {
	Some(token) => token,
	_ => Token::IDENT(literal)
    }
}
