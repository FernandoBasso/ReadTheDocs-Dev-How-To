# Haskell Function Composition

```
λ> :type (.)
(.) :: (b -> c) -> (a -> b) -> a -> c

λ> :info (.)
(.) :: (b -> c) -> (a -> b) -> a -> c 	-- Defined in ‘GHC.Base’
infixr 9 .
```

`sum` takes a list of numbers and returns a single number:

```
λ> xs = [1..5]

λ> sum xs
15
```

If we want to compose `sum` with some other function, the other function must be able to take a number as argument. `id` takes anything, so, it takes a number.

```
λ> (id . sum) xs
15
```

It did nothing interesting, but it worked! `id` took 15, the result of `sum xs` and returned that same 15, as it should.

```
λ> (negate . sum) xs
-15

λ> negate (sum [1..5])
-15
```



## Composing three functions

Here, we get the result of `sum` (a single number), turn it into an array, which becomes `[15]`. Next, compose all three functions. If we ask the sum of `[1..5]`, it is 15. Apply `toArray` to 15 and we get `[15]`, and the length of an array with one value is 1 🙂.

```
λ> toArray v = [v]

λ> (toArray . sum) [1..5]
[15]

λ> (length . toArray . sum) [1..5]
1

-- Same as:
λ> length (toArray (sum [1..5]))
1
```



```
λ> filter even [1..5]
[2,4]

λ> filterEven = filter even

λ> length . filterEven $ [1..5]
2

-- Same as:
λ> length (filterEven [1..5])
2
```



```
λ> reverse [1..10]
[10,9,8,7,6,5,4,3,2,1]

λ> take 5 . reverse $ [1..10]
[10,9,8,7,6]
```



```
λ> take 5 . enumFrom $ 3
[3,4,5,6,7]

λ> take 5 (enumFrom 3)
[3,4,5,6,7]

λ> take 7 . filter odd . enumFrom $ 9
[9,11,13,15,17,19,21]

λ> sum . take 7 . filter odd . enumFrom $ 9
105
```



## References

* [Function Composition - Haskell Wiki](https://wiki.haskell.org/Function_composition)
* Book Haskell From First Principles, Chapter 07 → More Functional Patterns → Function Composition.