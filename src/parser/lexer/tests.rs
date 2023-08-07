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
\"foobar\"
\"foo bar\"
"
    .to_string();

    use token::Token::*;
    let tokens = vec![
        Let,
        Ident("five".to_owned()),
        Assign,
        Int("5".to_owned()),
        Semicolon,
        Let,
        Ident("ten".to_owned()),
        Assign,
        Int("10".to_owned()),
        Semicolon,
        Let,
        Ident("add".to_owned()),
        Assign,
        Function,
        LParen,
        Ident("x".to_owned()),
        Comma,
        Ident("y".to_owned()),
        RParen,
        LBrace,
        Ident("x".to_owned()),
        Plus,
        Ident("y".to_owned()),
        Semicolon,
        RBrace,
        Semicolon,
        Let,
        Ident("result".to_owned()),
        Assign,
        Ident("add".to_owned()),
        LParen,
        Ident("five".to_owned()),
        Comma,
        Ident("ten".to_owned()),
        RParen,
        Semicolon,
        Bang,
        Minus,
        Slash,
        Asterisk,
        Int("5".to_owned()),
        Semicolon,
        Int("5".to_owned()),
        LessThan,
        Int("10".to_owned()),
        GreaterThan,
        Int("5".to_owned()),
        Semicolon,
        If,
        LParen,
        Int("5".to_owned()),
        LessThan,
        Int("10".to_owned()),
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
        Int("10".to_owned()),
        Equal,
        Int("10".to_owned()),
        Semicolon,
        Int("10".to_owned()),
        NotEqual,
        Int("9".to_owned()),
        Semicolon,
        String_("foobar".to_owned()),
        String_("foo bar".to_owned()),
        Eof,
    ];

    let mut lexer = Lexer::new(input.chars().collect());

    for expected_token in tokens {
        assert_eq!(expected_token, lexer.next_token());
    }
}
