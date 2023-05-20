# Writing an interpreter in rust

I finally have the E in REPL hooked in to the loop.  It's a very basic E.  Only
supporting the basic operations.

## Math operations with literals

```Monkey
1 + 1
1 - 1
1 * 1
1 / 1
```

## Logic operations with literals

```Monkey
true != false;
true == true;
```

## if statements

```Monkey
if ( condition ) {
  a statement;
} else {
 a different statement;
}
```

Non-zero integers are considered true.

No support for variables yet
