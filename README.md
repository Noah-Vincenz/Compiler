# WHILE Compiler

A compiler for the WHILE language, written in Scala. It lexes, parses, and generates Java bytecode via the [Jasmin](http://jasmin.sourceforge.net/) assembler.

Built as coursework at King's College London.

## Architecture

The compiler has three stages:

1. **Lexer** — Brzozowski derivative-based regex matcher that tokenises source code into keywords, identifiers, numbers, operators, and strings
2. **Parser** — Combinator parser that builds an AST from the token stream
3. **Code Generator** — Traverses the AST and emits Jasmin assembly instructions, which are then assembled into JVM bytecode

## The WHILE Language

```
write "Fact";
read n;
a := 1;
while n > 1 do {
  a := a * n;
  n := n - 1
};
write "Result";
write a
```

### Supported features

- Variables and integer arithmetic (`+`, `-`, `*`)
- Boolean comparisons (`=`, `!=`, `<`, `>`)
- `if`/`then`/`else` conditionals
- `while`/`do` loops
- `for`/`upto`/`do` loops
- `read` (stdin) and `write` (stdout) for integers and strings
- `skip` (no-op)

## Usage

```bash
# compile and run with Scala 2
scala compiler.scala

# this compiles the example program to Jasmin assembly (fact.j),
# then assembles it to JVM bytecode (fact.class)
# run the compiled program with:
java fact
```

Requires Jasmin (`jvm/jasmin-2.4/jasmin.jar`) to be present in the working directory for the assembly step.

## Example Programs

The file includes three built-in example programs:

- **Factorial** (`progFact`) — reads a number and computes its factorial
- **Fibonacci** (`progFib`) — reads a number and computes the nth Fibonacci number
- **For loop** (`progFor`) — demonstrates the `for`/`upto` loop construct
