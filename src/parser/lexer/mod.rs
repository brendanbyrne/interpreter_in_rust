mod token;
pub use token::Token;

pub struct Lexer {
    input: Vec<char>,
    pub position: usize,
    pub read_position: usize,
    pub ch: char,
}

const NULL: char = '\u{0}';

fn is_letter(ch: char) -> bool {
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

fn is_whitespace(ch: char) -> bool {
    ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
}

impl Lexer {
    pub fn new(input: Vec<char>) -> Self {
        let mut lexer = Self {
            input,
            position: 0,
            read_position: 0,
            ch: NULL,
        };
        lexer.read_char();
        lexer
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        self.input[position..self.position]
            .to_vec()
            .iter()
            .collect()
    }

    fn read_int(&mut self) -> String {
        let position = self.position;
        while is_digit(self.ch) {
            self.read_char();
        }
        self.input[position..self.position]
            .to_vec()
            .iter()
            .collect()
    }

    fn skip_whitespace(&mut self) {
        while is_whitespace(self.ch) {
            self.read_char();
        }
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            return NULL;
        }
        return self.input[self.read_position];
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = NULL;
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        let tok: Token;

        self.skip_whitespace();

        match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    tok = Token::Equal;
                } else {
                    tok = Token::Assign;
                }
            }
            ';' => {
                tok = Token::Semicolon;
            }
            '(' => {
                tok = Token::LParen;
            }
            ')' => {
                tok = Token::RParen;
            }
            ',' => {
                tok = Token::Comma;
            }
            '+' => {
                tok = Token::Plus;
            }
            '{' => {
                tok = Token::LBrace;
            }
            '}' => {
                tok = Token::RBrace;
            }
            '-' => {
                tok = Token::Minus;
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    tok = Token::NotEqual;
                } else {
                    tok = Token::Bang;
                }
            }
            '*' => {
                tok = Token::Asterisk;
            }
            '/' => {
                tok = Token::Slash;
            }
            '<' => {
                tok = Token::LessThan;
            }
            '>' => {
                tok = Token::GreaterThan;
            }
            NULL => {
                tok = Token::EOF;
            }
            _ => {
                if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    return token::lookup_identifier(literal);
                } else if is_digit(self.ch) {
                    let literal = self.read_int();
                    return Token::Int(literal);
                } else {
                    tok = Token::Illegal(self.ch);
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
            EOF,
        ];

        let mut lexer = Lexer::new(input.chars().collect());

        for expected_token in tokens {
            assert_eq!(expected_token, lexer.next_token());
        }
    }
}
