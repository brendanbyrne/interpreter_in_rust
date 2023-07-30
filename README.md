# Writing an interpreter in rust

## TODO
- [ ] Convert lexer and parser to work on streams/iters
- [ ] Come to a conclusion on if Noop is necessary, or just null
- [ ] Make `parse_program` work with str& and String

## REPL Loop
I finally have the REPL hooked into the loop. It supports the following
operations.

### Math operations with literals

```Monkey
1 + 1
1 - 1
1 * 1
1 / 1
```

### Logic operations with literals

```Monkey
true != false;
true == true;
```

### if statements

Non-zero integers are considered true.

```Monkey
if ( expression ) {
  expression
} else {
  expression
}
```

### return statements

```Monkey
return expression;
```

```Monkey
if (true) {
  return 1;
}

not_reached;
```

### variables

```Monkey
let x = 5;
x; // 5
```

### functions

```Monkey
let indentity = fn(x) { x; };
indentity(5); // 5
```

### closures

```Monkey
let new_adder = fn(x) { fn(y) { x + y; }; };
let add_two = new_adder(2);
add_two(5); // 7
```

### Not supported yet

* error handling
