use super::*;

#[test]
fn next_token() {
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
"
    .to_string();

    use token::Token::*;
    let tokens = vec![
        Let,
        Ident("five".to_string()),
        Assign,
        Int("5".to_string()),
        Semicolon,
        Let,
        Ident("ten".to_string()),
        Assign,
        Int("10".to_string()),
        Semicolon,
        Let,
        Ident("add".to_string()),
        Assign,
        Function,
        LParen,
        Ident("x".to_string()),
        Comma,
        Ident("y".to_string()),
        RParen,
        LBrace,
        Ident("x".to_string()),
        Plus,
        Ident("y".to_string()),
        Semicolon,
        RBrace,
        Semicolon,
        Let,
        Ident("result".to_string()),
        Assign,
        Ident("add".to_string()),
        LParen,
        Ident("five".to_string()),
        Comma,
        Ident("ten".to_string()),
        RParen,
        Semicolon,
        Bang,
        Minus,
        Slash,
        Asterisk,
        Int("5".to_string()),
        Semicolon,
        Int("5".to_string()),
        LessThan,
        Int("10".to_string()),
        GreaterThan,
        Int("5".to_string()),
        Semicolon,
        If,
        LParen,
        Int("5".to_string()),
        LessThan,
        Int("10".to_string()),
        RParen,
        LBrace,
        Return,
        True,
        Semicolon,
        RBrace,
        Else,
        LBrace,
        Return,
        False,
        Semicolon,
        RBrace,
        Int("10".to_string()),
        Equal,
        Int("10".to_string()),
        Semicolon,
        Int("10".to_string()),
        NotEqual,
        Int("9".to_string()),
        Semicolon,
        Eof,
    ];

    let mut lexer = Lexer::new(input.chars().collect());

    for expected_token in tokens {
        assert_eq!(expected_token, lexer.next_token());
    }
}
