# Rewriting functions using folds

- [Rewriting functions using folds](#rewriting-functions-using-folds)
  - [and](#and)
    - [Direct recursion, not point-free](#direct-recursion-not-point-free)
    - [Folding and point-free](#folding-and-point-free)
  - [or](#or)
    - [Direct recursion, not point free](#direct-recursion-not-point-free-1)
    - [Direct recursion, point-free](#direct-recursion-point-free)
  - [any](#any)
    - [Direct recursion, not point-free](#direct-recursion-not-point-free-2)
    - [Type checking problem](#type-checking-problem)
    - [Composing with f and map](#composing-with-f-and-map)
    - [Composing (||) and f](#composing-and-f)
  - [elem](#elem)
    - [Using fold and a lambda](#using-fold-and-a-lambda)
    - [Using any](#using-any)
  - [reverse](#reverse)
    - [foldr and lambda](#foldr-and-lambda)
    - [foldl and flip (:)](#foldl-and-flip-)
  - [map](#map)
    - [using foldr and lambda](#using-foldr-and-lambda)
  - [filter](#filter)
    - [using foldr and guards](#using-foldr-and-guards)
    - [using foldr and case of](#using-foldr-and-case-of)
    - [using foldr with if else](#using-foldr-with-if-else)
  - [squish (flatten)](#squish-flatten)
    - [using foldr](#using-foldr)
  - [squishMap](#squishmap)
  - [using foldr](#using-foldr-1)
  - [squishAgain](#squishagain)
  - [myMinimumBy, myMaximumBy](#myminimumby-mymaximumby)
    - [foldr and if else](#foldr-and-if-else)
    - [foldr and case of](#foldr-and-case-of)

## and

### Direct recursion, not point-free

```hs
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd_r xs
```

Using `foldr`, not point free.
```hs
myAnd :: [Bool] -> Bool
myAnd = foldr (\e acc -> e && acc) True
```

### Folding and point-free

`(&&)` is the folding function and `True` is the *zero* (initial value for the accumulator). We partially apply `foldr` with the first two params. The remaining one, the list to be operated on is still missing, and it is not defined as a parameter to `myAnd`. There is no `myAnd xs = ...`. That is what makes it *point free*. The type signature denounces that wee need a list of bool, though, and return a bool.

```hs
myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True
```

## or

### Direct recursion, not point free

```hs
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs
```

@TODO: Why does the author say it is **not** point free? We are not declaring that `myOr` explicitly takes a list, like `myOr xs = ...`.

To me, this is point-free.

```hs
myOr :: [Bool] -> Bool
myOr = foldr (\e acc -> e || acc) False
```

Or, the folding function is not point-free, because it takes explicit params `e` and `acc`.

This is not point free because we are explicitly declaring the param `xs`.

```hs
myOr :: [Bool] -> Bool
myOr xs = foldr (\e acc -> e || acc) False xs
```

### Direct recursion, point-free

And the fully point-free version using folds.

```hs
myOr :: [Bool] -> Bool
myOr = foldr (||) False
```

“Point” in *point-free* refers to variables. Variables are the *points*. If a function mentions variables, it is not point-free.

## any

### Direct recursion, not point-free

```hs
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs
```

### Type checking problem

This version works if we comment the type signature for the inner function `g`. That type definition seems totally OK but it doesn't type check.

```hs
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr g False
  where
    -- g :: a -> Bool -> Bool
    g = \ x acc -> f x || acc
```

In the  definition of `g` we reference a variable that appears in the signature of `myAny`. But those `a`s are different. We could enable `ScopedTypeVariables` extension, though, which makes those `a`'s be the same.

Also, we see a compiler error mentioning `a1`, which is the name to compiler gives to the `a` polymorphic type variable in `g` to differentiate it from the `a` polymorphic type variable in `myAny`. This works:

```hs
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}

myAny :: forall a. (a -> Bool) -> [a] -> Bool
myAny f = foldr g False
  where
    g :: a -> Bool -> Bool
    g = \ e acc -> f e || acc
```

Thanks @infixl-1 (Izuzu#5593) for the [help on Discord](https://discord.com/channels/280033776820813825/796099254937845790/871017722761322516). He mentioned that “I usually reserve type signatures for top-level declarations because of shenanigans like this (which honestly aren't really shenanigans).”

### Composing with f and map

```hs
myAny :: (a -> Bool) -> [] a -> Bool
myAny f = foldr (||) False . map f
```

We can think of it like this:

We first `map f` over the list of values. Ex:

```hs
λ> map even [0, 2, 3]
[True,True,False]
```

Then, we `(||)` each on the results of the previous operation:

```GHCi
λ> foldr (||) False [True, True, False]
True
```

**EXCEPT THAT BECAUSE OF LAZY EVALUATION, `map f` will only produce values as as needed by `foldr`**. It WILL NOT actually map over the entire input unless `foldr` folding function keeps asking for more values and ends up reaching the base case. As soon as `f` produces `True` for an element, `myAny` returns `True` and no more of the list is processed.

Thanks @konsumlamm for [this answer on Discord](https://discord.com/channels/280033776820813825/796099254937845790/871041627060334632).

### Composing (||) and f

This is a suggestions from the linter. Easier to understand!

```hs
myAny :: (a -> Bool) -> [] a -> Bool
myAny f = foldr ((||) . f) False
```

## elem

### Using fold and a lambda

```hs
myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\x acc -> x == e || acc) False
```

### Using any

Partially applying `any`. Also partially applying `==`.

```hs
myElem :: Eq a => a -> [a] -> Bool
myElem e = any (e ==)
```

## reverse

### foldr and lambda

Inside the lambda, we keep concatenating the element to the end of the list. That is what reverses it.

```hs
myRev :: [a] -> [a]
myRev = foldr (\e acc -> acc ++ [e]) []
```

### foldl and flip (:)

```hs
myRev :: [a] -> [a]
myRev = foldl (flip (:)) []
```

Let's try to understand what is going on here.

```GHCi
λ> f = flip (:)

λ> :t f
f :: [a] -> a -> [a]

λ> :t (:)
(:) :: a -> [a] -> [a]

λ> [] `f` 1
[1]

λ> 1 : []
[1]
```

Both `:` and `f` add an element to the beginning of the list. But whereas `:` takes an element and a list, `f` takes a list and an element.

Not here that the list comes first, then the infix `f`, then the element:

```GHCi
λ> [1, 2] `f` 3
[3,1,2]
```

A simple implementation of `foldl` to help visualize:

```hs
foldl :: (b -> a -> b) -> b -> [] a -> b
foldl _ z [] = z
foldl f z (x:xs) = foldl f (f z x) xs
```

And a definition of `myRev` using `foldl` and `f` (which is `flip (:)`):

Here's how evaluation goes:

```GHCi
myRev = foldl f []

myRev [1, 2, 3]
foldl f ([] `f` 1) [2, 3]
→ Add 1 to the front of the acc []
→ acc now is [1]

foldl f ([1] `f` 2) [3]
→ Add 2 to the front of the acc [1]
→ acc now is [2, 1]

foldl f ([2, 1] `f` 3) []
→ Add 3 to the front of the acc [2, 1]
→ acc now is [3, 2, 1]
```

## map

### using foldr and lambda

```hs
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\e acc -> f e : acc) []
```

In `f e : acc`, we apply `f` to `e` producing the new value, which is then consed into `acc`.

## filter

### using foldr and guards

```hs
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr go []
  where
    go e acc
      | f e = e : acc
      | otherwise = acc
```

We could replace this line:

```
myFilter f elems = foldr go [] elems
```

```
myFilter f = foldr go []
```

That is, we partially apply `foldr` to the folding function and the accumulator, but omit the list of elements to be processed. This way, we can also omit the list of elements from `myFilter f xs`, making it just `myFilter f`.

### using foldr and case of

This indentation looks ugly.

```hs
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f =
  foldr (\e acc
    -> case f e of
        True -> e : acc
        _    -> acc) []
```

Perhaps, instead of a lambda using a run function looks better?

```hs
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr run []
  where
    run e acc =
      case f e of
        True -> e : acc
        _    -> acc
```

### using foldr with if else

This is a suggestion from the linter:

```hs
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr run []
  where
    run e acc = if f e then e : acc else acc
```

I still use the run function pattern above. We can also follow the linter suggestion but use a lambda and not the run function.

```hs
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f =
  foldr(\e acc -> if f e then e : acc else acc) []
```

## squish (flatten)

`squish` is a function that flattens a list:

```GHCi
λ> squish [[1], [2], [3]]
[1,2,3]

λ> concat [[1], [2], [3]]
[1,2,3]

λ> squish ["Tomb", "Raider", "1996"]
"TombRaider1996"
```

The built-in `concat` does the same thing.

### using foldr

```hs
squish :: [[a]] -> [a]
squish = foldr (\xs acc -> xs ++ acc) []
```

Can be simplified and shortened to:

```hs
squish :: [[a]] -> [a]
squish = foldr (++) []
```

## squishMap

## using foldr

```hs
--
-- Maps a function over a list and concatenates the results.
--
-- From the example usage in the book, we must make sure our
-- `f` function takes one element, does whatever it wants with
-- it, and returns it as a list.
--
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\e acc -> f e ++ acc) []

f :: Char -> [Char]
f c = "[ " ++ [c] ++ " ]"
--
-- λ> squishMap f "jedi"
-- "[ j ][ e ][ d ][ i ]"
--

g :: Int -> [Int]
g i = [i + 1]
--
-- λ> squishMap g [0, 2, 4]
-- [1,3,5]
--
```

## squishAgain

Using the previously defined `squishMap`.

```hs
squishAgain :: [[a]] -> [a]
squishAgain = squishMap (\e -> e)
```

```GHCi
λ> squishAgain [[1], [2], [3]]
[1,2,3]

λ> squish
squishAgain  squishMap
λ> squishAgain ["Tomb", " ", "Raider", " ", "1996"]
"Tomb Raider 1996"
```

`squishMap` concatenates the result, which is what we want, but it also applies a function to each element it operates on before concatenation. The problem is that we don't want to do anything with each argument besides concatenating it to the accumulator in order to *flatten* it. That is where the `identity` function comes in. It satisfies `squishMap` requirement for a function argument, but it just returns the element unmodified. This way, we just flatten the list without modifying the inputs.

In short, we need the *id* function to satisfy `squishMap` here. `\e -> e` is the identity function. Could be replaced with `id`.

## myMinimumBy, myMaximumBy
### foldr and if else

```hs
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy p xs = foldr (\x acc ->
                          if p x acc == GT
                          then x
                          else acc) (last xs) xs

myMininumBy :: (a -> a -> Ordering) -> [a] -> a
myMininumBy p xs = foldr (\x acc ->
                            if p x acc == LT
                            then x
                            else acc) (last xs) xs
```

### foldr and case of

```hs
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr fn (last xs) xs
  where
    fn e acc =
      case f e acc of
        GT -> e
        _  -> acc

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr fn (last xs) xs
  where
    fn e acc =
      case f e acc of
        LT -> e
        _  -> acc
```

Here, our comparison function (the lambda) always says `LT`, so, never mind that 1 is actually less than 2 😅. We are forcing the function to lie.

```GHCi
λ> myMinimumBy (\_ _ -> LT) [1..5]
5
```

But if we use a proper comparison function, then all is fine:

```GHCi
λ> myMinimumBy compare [1..5]
1

λ> myMaximumBy compare [1..5]
5
```
