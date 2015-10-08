# Postfix
Evaluates expressions in postfix (reverse polish) notation.

## Build
```
$ scalac Evaluator.scala
```

## Run
```
$ scala Evaluator ...
```

## Operands
Only integers.

## Operators
`+`
`-`
`*`
`/`

## Examples

```
$ scala Evaluator 1 2 +
3
```

```
$ scala Evaluator 9 8 \* 47 3 / +
87
```

Note: \\* is necessary to prevent substitution by the shell.
