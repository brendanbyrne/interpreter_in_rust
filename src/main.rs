use std::io::{self, Write};

mod parser;
use parser::lexer::Lexer;
use parser::{ast, Parser};

const PROMPT: &str = ">> ";

fn read() -> io::Result<ast::Program> {
    print!("{}", PROMPT);
    io::stdout().flush()?;

    let mut line = String::new();
    io::stdin().read_line(&mut line)?;

    let lexer = Lexer::new(line.trim().chars().collect());
    let program = Parser::new(lexer).parse_program();

    println!("Parsed program:\n{}", program.to_string());

    Ok(program)
}

fn eval(program: ast::Program) {}

fn main() -> io::Result<()> {
    println!("Monkey progamming language interpreter");
    loop {
        let program = read()?;
        eval(program);
        // let results = eval(program);
        // print(results);
    }
}
