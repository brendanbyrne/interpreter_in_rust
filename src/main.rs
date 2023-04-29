use std::io::{self, Write};

mod lexer;
use lexer::{Lexer, Token};

const PROMPT: &str = ">> ";

fn main() -> io::Result<()> {
    println!("Monkey progamming language interpreter"); 
    loop {
	print!("{}", PROMPT);
	io::stdout().flush()?;
	
	let mut line = String::new();
	io::stdin().read_line(&mut line)?;

	let mut lexer = Lexer::new(line.trim().chars().collect());
	let mut token = lexer.next_token();
	while token != Token::EOF {
	    println!("{:?}", token);
	    token = lexer.next_token();
	}
	
    }
    // Ok(())
}
