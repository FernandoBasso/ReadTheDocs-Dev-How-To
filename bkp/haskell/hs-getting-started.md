# Haskell Getting Started

## Intro

We need GHC and GHCi.

```com
$ sudo pacman -S ghc
```

We have this file `hello.hs`:

```haskell
sayHello :: String -> IO ()
sayHello s =
  putStrLn ("Hello, " ++ s ++ "!")
```

Run it like this:

```
$ ghci hello.hs 

λ> sayHello "Lara"
Hello, Lara!
```

Or load the file *after* you are in a GHCi REPL session:

```
$ ghci
λ> :load hello.hs
λ> sayHello "Yoda"
Hello, Yoda!
```

For Emacs and Haskell Mode see hs-emacs.md.

## GHCi Tips

To catch **partial functions** during development (rather than at runtime), enable `-Wall`:

```
:set -Wall
```

Then, when you load a file into the repl, you'll see something like:

```
dayweek.hs:(15,3)-(21,23): warning: [-Wincomplete-patterns] …
    Pattern match(es) are non-exhaustive
    In an equation for ‘==’:
        Patterns not matched:
            Mon Tue
            Mon Weds
            ...
```

## Thinking in Haskell, General Tips

### Types, associativity, infixl, infixr, precedence

It is very important when trying to learn some new thing or function to actually pay attention to both the type and the infix l/r and precedence. Sometimes, the function type alone is not enough to help us understand it, or how to use it, until the prefix l/r and precedence thing are also taken into consideration.

As an example, take a look at [this post about dollar sign \$ from typeclasses.com](https://typeclasses.com/featured/dollar):

![Look at infix prefix precedence](./haskell/hs-look-infix-prefix-precedence.png)

Read the entire post carefully!

## Unit Testing

* https://lexi-lambda.github.io/blog/2016/10/03/using-types-to-unit-test-in-haskell/
* 