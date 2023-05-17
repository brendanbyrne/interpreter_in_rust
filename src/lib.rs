//! # Monkey Interpreter Crate
//!
//! `monkey_interpreter` is an implementation of the Monkey programming
//! language running in an interpreted environment.  Getting started is as
//! simple as calling `start()`

use std::io::{self, Write};

mod parser;
use parser::lexer::Lexer;
use parser::{ast, Parser};

pub const MONKEY_FACE: &'static str = r#"
            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

const PROMPT: &'static str = ">> ";

/// Starts the interpreter's REPL cycle
pub fn start() -> io::Result<()> {
    println!("Monkey progamming language interpreter");

    loop {
        let program = read()?;
        eval(program);
        // let results = eval(program);
        // print(results);
    }
}

/// Read the users input and attempt to interpret it as Monkey code
fn read() -> io::Result<ast::Program> {
    print!("{}", PROMPT);
    io::stdout().flush()?;

    let mut line = String::new();
    io::stdin().read_line(&mut line)?;

    let lexer = Lexer::new(line.trim().chars().collect());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if !parser.errors.is_empty() {
        println!("{}", MONKEY_FACE);
        println!("Ran into some monkey business.");
        println!("The following errors occured while parsing:");
        for msg in parser.errors {
            println!("{}", msg);
        }
    }

    println!("Parsed program:\n{}", program.to_string());

    Ok(program)
}

/// Evaluate the parsed Monkey program
fn eval(program: ast::Program) {}
