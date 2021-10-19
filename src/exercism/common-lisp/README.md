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
