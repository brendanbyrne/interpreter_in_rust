///! The kinds of tokens that are allowed by Monkey's lexer

/// Valid tokens in Monkey
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Illegal(char),
    EOF,
    Ident(String),
    Int(String),
    Assign,
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LessThan,
    GreaterThan,
    Equal,
    NotEqual,
}

fn keyword_check(literal: &str) -> Option<Token> {
    match literal {
        "let" => Some(Token::Let),
        "fn" => Some(Token::Function),
        "true" => Some(Token::True),
        "false" => Some(Token::False),
        "if" => Some(Token::If),
        "else" => Some(Token::Else),
        "return" => Some(Token::Return),
        _ => None,
    }
}

/// Returns either a keywork or indentifier token
pub fn lookup_identifier(literal: String) -> Token {
    if let Some(token) = keyword_check(&literal) {
        return token;
    }
    Token::Ident(literal)
}
