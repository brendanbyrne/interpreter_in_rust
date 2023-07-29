# Writing an interpreter in rust

## TODO
- [ ] Convert lexer and parser to work on streams/iters
- [ ] Come to a conclusion on if Noop is necessary, or just null
- [ ] Make `parse_program` work with str& and String

## REPL Loop
I finally have the E in REPL hooked in to the loop.  It's a very basic E.  Only
supporting the basic operations.

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

### Not supported yet

* variables
* error handling
