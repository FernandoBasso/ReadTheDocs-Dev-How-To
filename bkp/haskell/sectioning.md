# Haskell Sectioning

When using sectioning with commutative functions, the side in which the operator is placed makes no difference because the order of the arguments does not change the result.

```ghci
λ> (2+) 3
5
λ> (+2) 3
5
```

But when the function is not commutative, like `(^)`, then it does change the results.

```ghci
λ> (2^) 3
8
λ> (^2) 3
9
```

`2 ^ 3` is 8, but `3 ^ 2` is 9.

```ghci
λ> (1/) 2
0.5
λ> (/1) 2
2.0
```

## Subtraction vs Negation
