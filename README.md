[![Haskell CI](https://github.com/DrearyLisper/lambda/actions/workflows/haskell.yml/badge.svg)](https://github.com/DrearyLisper/lambda/actions/workflows/haskell.yml)

# Lamba calculus interpreter

![Logo](https://raw.githubusercontent.com/DrearyLisper/lambda/master/images/logo.png)

This interpreter patially implements lambda-calculus reduction rules.
It supports evaluation of names, functions, and function applications.

Functions should be defined using *\* symbol.
```
λ> \x.x
\x.x
```

To evaluate function use "(" ")".
```
λ> (\x.x y)
y
```
