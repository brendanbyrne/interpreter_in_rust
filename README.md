# monkey-interpreter

This repo is documents my experiences working through the "Writing an Interpreter in Go" book...in rust.

## Current goal
Make `parse_program` work with str& and String.

## Supported Language Features
- Math and logic operations
- Variable assignment
- Conditionals
- functions
- return statements
- closures

## Not supported yet
- Error handling

## Some sample code
```rust
let foo = 42;
let bar = 5;

let test_func = fn(x,y)
{
  if (foo + bar > 47) {
    return true;
  }
  else{
    return test_func(x, y + 1);
  }
```
