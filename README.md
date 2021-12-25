[![Haskell CI](https://github.com/DrearyLisper/lambda/actions/workflows/haskell.yml/badge.svg)](https://github.com/DrearyLisper/lambda/actions/workflows/haskell.yml)

# Lamba calculus interpreter

![Logo](https://raw.githubusercontent.com/DrearyLisper/lambda/master/images/logo.png)

This interpreter patially implements lambda-calculus reduction rules.

``` haskell
> TERM=dumb cabal run
Up to date
Welcome to Lambda REPL!
λ> (\x.x \x.x)
0: Application (Function (Name "x") (Name "x")) (Function (Name "x") (Name "x"))
1: Function (Name "x") (Name "x")
2: Function (Name "x") (Name "x")
3: Function (Name "x") (Name "x")
4: Function (Name "x") (Name "x")
λ> (\first.\second.first \x.x)
0: Application (Function (Name "first") (Function (Name "second") (Name "first"))) (Function (Name "x") (Name "x"))
1: Function (Name "second") (Function (Name "x") (Name "x"))
2: Function (Name "second") (Function (Name "x") (Name "x"))
3: Function (Name "second") (Function (Name "x") (Name "x"))
4: Function (Name "second") (Function (Name "x") (Name "x"))
λ> ((\first.\second.first \x.x) \y.y)
0: Application (Application (Function (Name "first") (Function (Name "second") (Name "first"))) (Function (Name "x") (Name "x"))) (Function (Name "y") (Name "y"))
1: Function (Name "x") (Name "x")
2: Function (Name "x") (Name "x")
3: Function (Name "x") (Name "x")
4: Function (Name "x") (Name "x")
λ> 
Goodbye!
```
