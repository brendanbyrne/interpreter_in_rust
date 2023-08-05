# monkey-interpreter

This repo documents my experiences working through the "Writing an Interpreter in Go" book...in rust.

## Current goals
1. Add support for `std::fmt::Display` trait to environment.
2. Make `parse_program` work with `str&` and `String`.

## Supported Language Features
- Math and logic operations
- Variable assignment
- Conditionals
- functions
- return statements
- closures

## How to use
1. Clone repo.
```
git clone https://github.com/brendanbyrne/monkey_interpreter.git
```
2. Run crate.
```
cargo run
```
3. Input a line to be interpreted, then press enter.
```
>> let example = 1337;
1337
```

## Not supported yet
- Error handling

## Some sample code
```rust
let foo = 42;
let bar = 5;
```

**Note: The interpreter doesn't support multiline inputs.  But the single line can be as long as you want.**
```rust
let func = fn(x,y)
{
  if (foo + bar > 47) {
    return true;
  }
  else{
    return func(x, y + 1);
  }
}
```

The above could be written as the following:

```rust
let func = fn(x,y) { if (foo + bar > 47) { return true; } else { return func(foo, bar+1); }
```