# Laziness, Evaluation, Precedence, Association

## Question about non-strictness on the left, right etc

[Link to my question about it](https://discord.com/channels/280033776820813825/505367988166197268/862992875377655809)

My question causes an endless discussion about many related topics and concepts.

```
myAny :: (a -> Bool) -> [] a -> Bool
myAny f xs = foldr (\x b -> f x || b) False xs
--
-- λ> myAny odd [2..]
-- True
--
```

We can pass undefined to see what happens. That gives an idea of how things may force, or not force, evaluation:

```
λ> True || undefined
True

λ> False || undefined
*** Exception: Prelude.undefined
```


```
λ> foldr (\_ _ -> 0) 0 [1, 2, 3]
0

λ> foldr (\_ _ -> 0) 0 [1, 2, undefined]
0

λ> foldr (\_ _ -> 0) 0 [undefined, 2, 3]
0

λ> foldr (\_ _ -> 0) 0 undefined ++ [undefined, 2, 3]

λ> foldr (\_ _ -> 0) 0 undefined
*** Exception: Prelude.undefined
```

Why does `foldr const 0 [1..]` return 1 and not zero?

Because `foldr` takes a function, an initial, default (zero, memo, accumulator) value, and a list of things.

`foldr const 0` means `const 0` is a function passed to `foldr`. Since we passed just  one argument to `const`, it returns a function awaiting final argument. So, `foldr` applies `const 0` to `[5, 2]`, and 5 becomes the `acc` of the fold, because `const` is non-strict, the `z` (5) is returned.

```
λ> foldr (+) 0 [1..3]
6

λ> foldr const 0 [5, 2]
5
```
