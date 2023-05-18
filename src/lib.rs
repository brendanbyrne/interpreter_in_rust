//! # Monkey Interpreter Crate
//!
//! `monkey_interpreter` is an implementation of the Monkey programming
//! language running in an interpreted environment.  Call `start()` to begin the
//! REPL sequence.

use std::error::Error;
use std::io::{self, Write};

mod parser;
use parser::{parse_program, Program};

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
///
/// Runs until SIGINT, [ctrl] + c, triggers its termination.
///
/// # Example
/// ```no_run
/// use monkey_interpreter;
/// monkey_interpreter::start();
/// ```
pub fn start() {
    println!("Monkey progamming language interpreter");

    loop {
        let program = match read() {
            Ok(program) => program,
            Err(error_msg) => {
                let error_msg = format!(
                    "{}
Ran into some monkey business.
The following errors occured while parsing:
{}",
                    MONKEY_FACE, error_msg
                );
                println!("{}", error_msg);
                continue;
            }
        };
        eval(program);
        // let results = eval(program);
        // println!(results.to_strin());
    }
}

/// Returns a program or the error message from the parser
///
/// Attempts to parse a program read from stdin
fn read() -> Result<Program, Box<dyn Error>> {
    print!("{}", PROMPT);
    io::stdout().flush()?;

    let mut line = String::new();
    io::stdin().read_line(&mut line)?;

    let program = parse_program(&line)?;

    Ok(program)
}

/// Evaluate the parsed program
fn eval(program: Program) {
    println!("Parsed program:\n{}", program.to_string());
}
