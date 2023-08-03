mod token;
pub use token::Token;

#[cfg(test)]
mod tests;

pub struct Lexer {
    input: Vec<char>,
    pub position: usize,
    pub read_position: usize,
    pub ch: char,
}

const NULL: char = '\u{0}';

fn is_letter(ch: char) -> bool {
    ('a'..='z').contains(&ch) || ('A'..='Z').contains(&ch) || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ('0'..='9').contains(&ch)
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
        self.input[self.read_position]
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
