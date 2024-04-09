# foldl const evaluation
TAGS: 2021-07-23 const foldl
```
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

foldr const 0 [1, 2, 3]
foldr const 1 (foldr const 0 [2, 3])
              ----------------------
                 rest of the fold
```

Here, the “rest of the fold” is never evaluated and evaluation returns 1 and stops.

`foldr` applies the function to the first argument of the list, which is 1. The second argument to `const` is the “rest of the fold”.

```
λ> :t const
const :: a -> b -> a

λ> :t (flip const)
(flip const) :: b -> c -> c
```

So, if we `flip const`, it will return the second argument instead.

```
f = flip const

foldr f 0 [1, 2, 3]
f 1 (foldr f 0 [2, 3]))
f 1 (f 2 (foldr f 0 [3]))
f 1 (f 2 (f 3 (foldr f 0 [])))
f 1 (f 2 (f 3 0)) -- <1>
f 1 (f 2 0)
f 1 0
0
```

## Foo

Because we flipped `const`, it will swap the params and force evaluation of the “rest of the fold”. When we reach <1>, and since the params are flipped, `const` returns the second argument instead of the first. We keep reducing the values in that same fashion and therefore arise at the result 0. 😮

```
λ> scanr const 0 [1, 2, 3]
[1,2,3,0]

λ> scanr (flip const) 0 [1, 2, 3]
[0,0,0,0]
```

------------------------------------------------------------------------------
```
f = flip (*)

foldl f 1 [1, 2, 3]
foldl f (f 1 1) [2, 3]
           1 (1 * 1 is 1)
foldl f (f 1 2) [3]
           2 (1 * 2 is 2)
foldl f (f 2 3) []
           6 (2 * 3 is 6)
6
```
------------------------------------------------------------------------------
```
foldl :: (b -> a -> b) -> b -> [] a -> b
foldl _ z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl ((++) . show) "" [1, 2, 3]
foldl ((++) . show) ((++) . show) "" 1) [2, 3]
```
------------------------------------------------------------------------------
```
foldr const 'z' [1, 2]
const 1 (foldr const 'z' [2])
const 1 (const 2 (foldr const 'z' []))
const 1 (const 2 'z')
const 1 'z'
1
```
In theory, it should work if we forget about types. But it does not typecheck! `const` allows for different types *a* and *b*, but it does not match with `foldr` type.

In the case above:

```
const :: a -> b -> a
const :: Int -> Char -> Int

foldr :: (a -> b -> b) ...
foldr :: (Int -> Char -> Char) ...
```
If we compare the types of `const (a -> b -> a)` and the function `foldr` takes `(a -> b -> b)`, you can see that `a` and `b` have to be the same type. It typechecks if `flip const`.

```
(flip const) :: b -> a -> a
```

------------------------------------------------------------------------------
## foldr const 0 "tacos"

0 is used as the accumulator, is the last value `foldr` will apply `const` to.

```
const 't' (foldr const 0 "acos")
const 't' (const 'a' (foldr const 0 "cos"))
...
```

`foldr` needs the type the accumulator to be a number because we passed 0 as the initial accumulator. But `const` will try to return its first argument, which is the char `'t'`. Yet, the folding function passed to `foldr` is expected to return something with the type of the accumulator. It does not typecheck.

```
λ> :type foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

λ> :type const
const :: a -> b -> a
```

If we `flip const`, then it returns something of the type of its second argument, and that matches the type expected by `foldr`. But then we get a different result. Let's exemplify with a shorter string.

```
f :: flip const
λ> :t f
f :: b -> c -> c
```

```
foldr f 0 "xyz"
f 'x' (foldr f 0 "yz")
f 'x' (f 'y' (foldr f 0 "z"))
f 'x' (f 'y' (f 'z' (foldr f 0 "")))
f 'x' (f 'y' (f 'z' 0))
f 'x' (f 'y' 0)
f 'x' 0
0
```

------------------------------------------------------------------------------
```
f = flip const
λ> :t f
f :: a -> b -> b

foldl :: (b -> a -> b) -> b -> [] a -> b
foldl _ z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl f 0 "abc"
```

It does not typecheck. f (our flipped const) takes an *a* ast the first argument, but the folding function for `foldl` expects a *b* as it first argument. The types don't match. It works if we do not `flip const` (contraty to `foldr`, with which we **have to** `flip const`.

Both `foldr` with unfliped `const` and `foldl` with flipped `const` do not
typecheck.

```
λ> foldr (flip const) 0 "xyz"
0

λ> foldl const 0 "xyz"
0

λ> foldr const 0 "xyz"
...ERROR...

λ> foldl (flip const) 0 "xyz"
...ERROR...
```

------------------------------------------------------------------------------
## flip const
TAGS: 2021-07-23
```
f = flip const
λ> :t f
f :: a -> b -> b

foldl :: (b -> a -> b) -> b -> [] a -> b
foldl _ z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl f 'z' [1, 2, 3]
```

Compare:

```
f     ::  a -> b -> b
foldl :: (b -> a -> b)
```

If `f` takes `Char -> Int -> Int`, then `foldl` folding function `(b -> a -> b)` takes `Char -> Int -> Char`. Note the first returns `Int`, while the second returns `Char`. They don't match and thus the failure in typechecking.


