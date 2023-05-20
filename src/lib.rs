//! # Monkey Interpreter Crate
//!
//! `monkey_interpreter` is an implementation of the Monkey programming
//! language running in an interpreted environment.  Call `start()` to begin the
//! REPL sequence.

use std::error::Error;
use std::io::{self, Write};

mod parser;
use parser::{parse_program, Program};

mod evaluator;
use evaluator::Evaluator;

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

    let mut evaluator = Evaluator::new();

    loop {
        let program = match read() {
            Ok(program) => program,
            Err(msg) => {
                let full_msg = format!(
                    "{}
Ran into some monkey business.
The following errors occured while parsing:
{}",
                    MONKEY_FACE, msg
                );
                println!("{}", full_msg);
                continue;
            }
        };

        match evaluator.eval(program) {
            Ok(obj) => println!("{}", obj.to_string()),
            Err(msg) => {
                println!("{}", msg);
                continue;
            }
        }
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
