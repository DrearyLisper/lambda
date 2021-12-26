[![Haskell CI](https://github.com/DrearyLisper/lambda/actions/workflows/haskell.yml/badge.svg)](https://github.com/DrearyLisper/lambda/actions/workflows/haskell.yml)

# Lamba calculus interpreter

![Logo](https://raw.githubusercontent.com/DrearyLisper/lambda/master/images/logo.png)

This interpreter patially implements lambda-calculus reduction rules.
It supports evaluation of names, functions, and function applications.

To start REPL use `cabal run lambda`
```
$ TERM=dumb cabal run lambda
Up to date
Welcome to Lambda REPL!
λ> (\s.(s s) \x.x)
\x.x
```

Functions should be defined using `\` symbol.
```
λ> \x.x
\x.x
```

To evaluate function use `(` `)`.
```
λ> (\x.x y)
y
```
