#[derive(PartialEq)]
#[derive(Debug)]
pub enum Token {
    ILLEGAL(char),
    EOF,
    IDENT(Vec<char>),
    INT(Vec<char>),
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
}

fn keyword_check(literal: String) -> Option<Token> {
    match literal.as_str() {
	"let" => Some(Token::LET),
	"fn" => Some(Token::FUNCTION),
	_ => None,
    }
}

pub fn lookup_identifier(literal: Vec<char>) -> Token {
    match keyword_check(literal.iter().collect()) {
	Some(token) => token,
	_ => Token::IDENT(literal)
    }
}
