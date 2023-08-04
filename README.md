# monkey-interpreter

This repo is documents my experiences working through the "Writing an Interpreter in Go" book...in rust.

## Current goal
Make `parse_program` work with str& and String.


## Features
- Math and logic operations
```rust
>> 42 + 5
47
```

```Monkey
1 + 1
1 - 1
1 * 1
1 / 1
```

### Logic operations with literals

```rust
true != false;
true == true;
```

### if statements

Non-zero integers are considered true.

```rust
if ( expression ) {
  expression
} else {
  expression
}
```

### return statements

```rust
return expression;
```

```rust
if (true) {
  return 1;
}

not_reached;
```

### variables

```rust
let x = 5;
x; // 5
```

### functions

```rust
let indentity = fn(x) { x; };
indentity(5); // 5
```

### closures

```rust
let new_adder = fn(x) { fn(y) { x + y; }; };
let add_two = new_adder(2);
add_two(5); // 7
```

### Not supported yet

* error handling
