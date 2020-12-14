# SIMPLI, the Simple IMP Language Interpreter

SIMPLI is an interpreter for the IMP language, a typical imperative language.

## Usage

There are four ways of using SIMPLI:

- Interpreting a source file
- Interpreting the standard input
- Interpreting a command
- Using it as a library

### Interpreting a source file

```sh
$ cat code.imp # The content of the input file
x := 3; y := x + 4
$ simpli code.imp
[x = 3,y = 7]
```

### Interpreting the standard input

```sh
$ echo "x := 3; y := x + 4" | simpli
[x = 3,y = 7]
```

### Interpreting a command

```sh
$ simpli -c "x := 3; y := x + 4"
[x = 3,y = 7]
```

### Using it as a library

Suppose you start a Hugs console inside the `src` directory of this repository:

```haskell
Hugs> :load Parsers
Parsers> eval "x := 3; y := x + 4"
[x = 3,y = 7]
```
