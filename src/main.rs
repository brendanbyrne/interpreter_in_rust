use std::io::{self, Write};

mod parser;
use parser::lexer::{Lexer, Token};

const PROMPT: &str = ">> ";

fn read() -> io::Result<String> {
    print!("{}", PROMPT);
    io::stdout().flush()?;
    
    let mut line = String::new();
    io::stdin().read_line(&mut line)?;
    Ok(line)
}
    
fn eval(line: String) {
    let mut lexer = Lexer::new(line.trim().chars().collect());
    let mut token = lexer.next_token();
    while token != Token::EOF {
	println!("{:?}", token);
	token = lexer.next_token();
    }
}

fn main() -> io::Result<()> {
    println!("Monkey progamming language interpreter"); 
    loop {
	let line = read()?;
	eval(line);
	// let results = eval(line);
	// print(results);
    }
}
