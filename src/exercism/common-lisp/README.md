# Exercism - Common Lisp

## Running in Emacs + SLIME REPL

Because of the way the code and tests are declared in packages, it
seems it is not possible to directly execute top-level functions in
the SLIME REPL.

When, for instance, we load a file with `C-c C-l` in the REPL, we have
to run its functions with something like:

```lisp
(name-of-the-package:name-of-the-function args)
```

An example from the “socks and sexprs” exercise:

```lisp
CL-USER> (socks-and-sexprs:is-an-atom-p 'z)
T
```

To run the tests, load the test file, again with something like `C-c
C-l` and then in the REPL do something like:

```lisp
CL-USER> (socks-and-sexprs-test:run-tests)
```

The output should be something like this:

```text
Running test suite SOCKS-AND-SEXPRS-SUITE
 Running test SYMBOLS ..
 Running test KEYWORDS .
 Running test ATOMS .............
 Running test CONSES ............
 Running test FIRST ..
 Running test REST .....
 Did 35 checks.
    Pass: 35 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)

T
NIL
NIL
CL-USER>
```

## Type Signatures

We try to use [Hindley Milner][hindley-milner] type signatures (and
type system) to describe our functions. Learn more about this on
the [Ramda Wiki on Type Signatures][ramda-wiki-type-signatures] and on
[Chapter 7][mostly-adequate-guide-FP] of The Mostly Adequate Guide to
Functional Programming book (free (as in speech)). Some examples
follow.

The function `f` takes an `Int` and returns a `Float`:

```text
f :: Int -> Float
```

The function `g` takes two chars and return a string. Note no `->`
between `Char` and `Char`.

```text
g :: Char Char -> String
```

The function `h` takes a char, and returns a function that takes a
char, which returns a string, that is, `h` is curried. Note the `->`
between `Char` and `Char`:

```text
h :: Char -> Char -> String
```

### Currying and Type Signatures

In Haskell, all functions are curried by default (as in Ramda
JavaScript library). In these cases, a function that like `add` in
curried in Haskell:

```
$ ghci
> add :: Int -> Int -> Int; add x y = ((+) x y)
λ> :type add
add :: Int -> Int -> Int
λ> add1 = add 1
λ> add1 10
11
```

As we we, we can indeed apply `add` to only one argument, which
returns a function that expects the other argument. That is currying
and partial application at work. Contrast these two type signatures:

```text
f :: Int -> Int -> Int
g :: Int Int -> Int
```

`f` signature means it takes an Int, which returns a function that
takes an Int and returns the final Int value. `f` is therefore curried.

`g` signature takes two Ints. It is **not** curried. We can't pass
just one argument. `g` requires both arguments at once.

[hindley-milner]: https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system
[ramda-wiki-type-signatures]: https://github.com/ramda/ramda/wiki/Type-Signatures
[mostly-adequate-guide-FP]: https://github.com/MostlyAdequate/mostly-adequate-guide/blob/master/ch07.md

