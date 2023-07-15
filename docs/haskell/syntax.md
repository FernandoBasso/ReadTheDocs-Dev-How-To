---
title: Syntax Tips, Ideas and Notes on Refactoring:: Haskell
description: Some ideas on different ways of writing code which results in the same (or similar enough) result.
tags: haskell syntax refactor
---

# Syntax

## concat to cons

Consider this implementation of `zipWith`:

```haskell
myZipWith :: (a -> b -> c) -> [] a -> [] b -> [] c
myZipWith _ [] _              = []
myZipWith _ _ []              = []
myZipWith f (x : xs) (y : ys) = [f x y] ++ myZipWith f xs ys
```

Note the `++` concat infix function on the last line.
Because we are using it, and it takes lists as arguments, we must enclose `f x y` inside braces to make the result of `f x y` become a list.

If we would switch from `++` concat to `:` cons, we don't need to wrap `f x y` with square braces, as we would then be “consing a value onto _more list_”:

```diff
- [f x y] ++ myZipWith f xs ys
+ f x y : myZipWith f xs ys
```

Function application has the highest precedence.
When `myZipWith` is applied to `f xs ys`, currying causes it to apply it to `f` only first, which returns a partially applied function that then is applied to `xs`, which again returns a partially applied function which is then finally applied to `ys`, meaning `myZipWith` is fully applied by now.
Only then is the `infixr 5` `:` cons constructor applied to the result of `f x y` and `myZipWith f xs ys`.
