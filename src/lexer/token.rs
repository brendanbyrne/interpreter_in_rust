#[derive(PartialEq)]
#[derive(Debug)]
pub enum Token {
    ILLEGAL,
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
