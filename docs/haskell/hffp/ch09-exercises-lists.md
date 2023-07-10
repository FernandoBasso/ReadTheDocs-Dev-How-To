---
title: ch09 List Exercises :: Haskell Programming From First Principles
description: Notes, solutions and explanations on the exercises from chapter 9 on lists for the book Haskell Programming From First Principles
tags: haskell hffp ch09 book exercise solution
---

# ch09 Exercises :: Lists

We can insert ⊥ (*bottom*) on Linux Gtk with `Ctrl+Shift+U` followed by `22a5`.
On Vim, in insert mode with `Ctrl+v u 22a5`.
On Emacs with `C-x u RET 22a5 RET`.

## Will it blow up?

Will the expressions return a value or ⊥ (*bottom*)?

```text
1. [x^y | x <- [1..5], y <- [2, undefined]]

2. take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]

3. sum [1, undefined, 3]

4. length [1, 2, undefined]

5. length $ [1, 2, 3] ++ undefined

6. take 1 $ filter even [1, 2, 3, undefined]

7. take 1 $ filter even [1, 3, undefined]

8. take 1 $ filter odd [1, 3, undefined]

9. take 2 $ filter odd [1, 3, undefined]

10. take 3 $ filter odd [1, 3, undefined]
```

### Exercise 01

```haskell
[x^y | x <- [1..5], y <- [2, undefined]]
```

The `undefined` from the second list will have to be evaluated as it is used as the exponent and this expression will return ⊥.

### Exercise 02

```haskell
take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
```

It will return the result of `1 ^ 2` and not ⊥.
This is because of `take 1` which will require the evaluation of the first pair only, not allowing evaluation to reach the `undefined`.

Because it is a list comprehension, it returns `1 ^ 2` (which is 1) inside a list, so the result is actually `[1]`.

### Exercise 03

```haskell
sum [1, undefined, 3]
```

Returns ⊥ because `sum` forces the values.

### Exercise 04

```haskell
length [1, 2, undefined]
```

Produces 3.
`length` only evals the spine, not forcing the values.

### Exercise 05

```haskell
length $ [1, 2, 3] ++ undefined
```

Produces ⊥.
Because of `++ undefined`, that `undefined` is part of the spine, and `length` needs to evaluate the spine.

NOTE: Of course, if we place that `undefined` in a value position (rather than in a spine position), then all is fine for `length`:

```text
λ> length $ [1, 2, 3] ++ [undefined]
4
```

### Exercise 06

```haskell
take 1 $ filter even [1, 2, 3, undefined]
```

Because of `take 1`, we’ll stop as soon as we find the even number `2` and we won’t reach `undefined`.
The result is `[2]`.

### Exercise 07

```haskell
take 1 $ filter even [1, 3, undefined]
```

Even though we have `take 1`, we don’t find an even number until we reach `undefined`, and `even` will force the value to check of its “evenness”, thus causing ⊥ to occur.

### Exercise 08

```haskell
take 1 $ filter odd [1, 3, undefined]
```

An odd number is found before reaching `undefined`.
The result is `[1].`

### Exercise 09

```haskell
take 2 $ filter odd [1, 3, undefined]
```

Two odd numbers are found just before reaching `undefined`.
The result is `[1, 3]`.

### Exercise 10

```haskell
take 3 $ filter odd [1, 3, undefined]
```

Two odd numbers are found, but not the third before reaching `undefined`, causing ⊥ to occur.

## Is it in normal form?

Page 494.

- [Question about these in Haskell Discord](https://discord.com/channels/280033776820813825/1119736989005123805).

For each expression below, determine whether it’s in:

1. Normal form, which implies weak head normal form.
2. Weak head normal form only.
3. Neither.

Remember that an expression cannot be in normal form or weak head normal form if the outermost part of the expression isn’t a data constructor.
It can’t be in normal form if any part of the expression is unevaluated.

**NOTE**: The `_` symbol in the examples below mean a value or expression has not been evaluated.

### Exercise 01

```haskell
[1, 2, 3, 4, 5]
```

NF and WHNF.

NF because no expression or sub-expression is un-evaluated.
WHNF because it is evaluated to at least the first/outermost data constructor (and by definition, anything in NF is also in WHNF).

### Exercise 02

```haskell
1 : 2 : 3 : 4 : _
```

WHNF.
Not all expressions have been evaluated due to the `_` symbol.

### Exercise 03

```haskell
enumFromTo 1 10
```

Neither because it is a function application to all possible arguments.

### Exercise 04

```haskell
length [1, 2, 3, 4, 5]
```

Neither.
It is a function application.

### Exercise 05

```haskell
sum (enumFromTo 1 10)
```

Neither.
It is a function application, which results in a value, which is also applied to a function.

### Exercise 06

```haskell
['a'..'m'] ++ ['n'..'z']
```

Neither.
The outermost part is a function application of `++`.

### Exercise 07

```haskell
(_, 'b')
```

WHNF.
The outermost part is the data constructor `(,)` even though not all sub-expressions are fully evaluated.

---

To be clear here what's super important is recognizing that WHNF and NF are syntactic properties of expressions
And in particular it's very easy to find out if an expression is in WHNF
Neither are properties of the values denoted by those expressions

---

## More Bottoms

Page 504.


### Exercise 01

```haskell
take 1 $ map (+1) [undefined, 2, 3]
```

Will be ⊥ because the first element we try to take and force is `undefined`, which is *bottom*.

### Exercise 02

```haskell
take 1 $ map (+1) [1, undefined, 3]
```

Yes,m it will return `[2]` from the evaluation of `(+ 1) 1`. We take 1 element before having to reach and try to force `undefined`.

### Exercise 03

```haskell
take 2 $ map (+1) [1, undefined, 3]
```

No, it will be ⊥ because we try have to force two elements from the list, and the second one is `undefined`.

![Bottom Undefined Error](/staticassets/ch09-exercise-bottom-undefined-error.png)

Note the part in red. It was able to evaluate at least the first value, but reached *bottom* when trying the second one.

### Exercise 04

What does the following mystery function do?
What is its type?
Describe it (to yourself or a loved one) in standard English and
then test it out in the REPL to make sure you are correct.

```haskell
itIsMystery xs = map (\x -> elem x "aeiou") xs
```

It returns a list of `Bool`.
`True` if the current element is a lowercase vowel, `False` otherwise.

It's type is `[Char] -> [Bool]`.

```text
λ> isMistery "hello"
[False,True,False,False,True]

λ> isMistery "abcde"
[True,False,False,False,True]
```

### Exercise 05

What will be the result of the following functions:

a. `map (^2) [1..10]`
b. `map minimum [[1..10], [10..20], [20..30]]` (n.b. `minimum` is not the same function ass the `min` function that we used before)
c. `map sum [[1..5], [1..5], [1..5]]`

#### a

Each element in the list will be raised to the second power:

```text
λ> map (^ 2) [1 .. 10]
[1,4,9,16,25,36,49,64,81,100]
```

#### b

It will return a list with the smallest (minimum) value of each sub-list.

```text
λ> minimum [1 .. 10]
1
λ> minimum [10 .. 20]
10
λ> minimum [20 .. 30]
20
λ> map minimum [[1 .. 10], [10 .. 20], [20 .. 30]]
[1,10,20]
```

#### c

It will return a list with three elements which sums `[1 .. 5]` in each case.

```text
λ> map sum [[1 .. 5], [1 .. 5], [1 .. 5]]
[15,15,15]
```

### Exercise 06

Back in Chapter 7, you wrote a function called foldBool.
That function exists in a module known as Data.Bool and is called bool.
Write a function that does the same (or similar, if you wish) as the map if-then-else function you saw above but uses bool instead of the if-then-else syntax.
Your first step should be bringing the bool function into scope by typing import Data.
Bool at your REPL prompt.

This is the “map if-then-else function we saw above”:

```text
λ> map (\x -> if x == 3 then (- x) else x) [1 .. 10]
[1,2,-3,4,5,6,7,8,9,10]
```

First, here’s how `bool` from `Data.Bool` works:

![Prelude Data.Bool as of 2023](/staticassets/prelulde-data-bool-2023.png)

It takes an argument of type $a$ and another argument of type $a$ and a `Bool`.
If the `Bool` is `False`, return the first argument; else return the second argument.

```text
λ> import Data.Bool (bool)
λ> :type bool
bool :: a -> a -> Bool -> a
λ> bool "Lara" "Croft" False
"Lara"
λ> bool "Lara" "Croft" True
"Croft"

λ> if not False then "Lara" else "Croft"
"Lara"
λ> if False then "Lara" else "Croft"
"Croft"
```

So, the solution:

```text
import Data.Bool (bool)

λ> map (\n -> bool n (- n) (n == 3)) [1 .. 5]
[1,2,-3,4,5]
```

`n` is the current number.
If `n == 3` is `False`, simply return `n`.
But if `n == 3` is `True`, then negate `n`.

## Exercises: Filtering

Page 335.

### Exercise 1

How might we write a filter function that would give us all the multiples of 3 out of a list from 1 to 30?

A multiple of 3 is a number whose remainder of it by 3 is 0.

One approach is to make a tiny helper function to check if a number is a multiple of three and then use it in the “main” function.

```haskell
isMultipleOf3 :: Int -> Bool
isMultipleOf3 n = rem n 3 == 0
```

```text
λ> filter isMultipleOf3 [1 .. 30]
[3,6,9,12,15,18,21,24,27,30]
```

### Exercise 2

Recalling what we learned about function composition, how
could we compose the above function with the length function
to tell us how many multiples of 3 there are between 1 and 30?

```haskell
isMultipleOf3 :: Int -> Bool
isMultipleOf3 n = rem n 3 == 0

countMultiplesOf3 :: [Int] -> Int
countMultiplesOf3 = length . filter isMultipleOf3
```

Of course, we’d rather do it more abstract and reusable:

```haskell
--
-- Checks whether y is a multiple of x.
--
isMultipleOf :: Integer -> Integer -> Bool
isMultipleOf x y = rem y x == 0

countMultiplesOf :: Integer -> [Integer] -> Int
countMultiplesOf n = length . filter (isMultipleOf n)
```

```text
λ> countMultiplesOf 3 [1 .. 30]
10
λ> countMultiplesOf 5 [1 .. 30]
6
λ> countMultiplesOf 10 [1 .. 30]
3
```

### Exercise 3

Next, we’re going to work on removing all articles (“the,” “a,” and “an”) from sentences.
You want to get to something that works like this:

```text
λ> myFilter "the brown dog was a goof"
["brown","dog","was","goof"]
```

Let’s assume these three string examples:

```haskell
snake :: [Char]
snake = "there a snake in here"

fruit :: [Char]
fuit = "an apple is a fruit"

jedi :: [Char]
jedi = "may the force be with you"
```

#### Solution 1

One approach is maybe create a tiny `isArticle` function that is then used in the “main” function:

```haskell
--
-- ASSUME: The input is all in lower case.
--
isArticle :: [Char] -> Bool
isArticle word = word == "a" || word == "an" || wrod "the"

--
-- ASSUME: The input is all in lowercase.
--
dropArticles :: [Char] -> [[Char]]
dropArticles str = filter (not . isArticle) (words str)
```

Note the `not . isArticle` composition.

```text
λ> dropArticles snake
["there","snake","here"]

λ> dropArticles fruit
["apple","is","fruit"]

λ> dropArticles jedi
["may","force","be","with","you"]
```

```{hint}
The type `[Char] -> [[Char]` is the same as `String -> [String]`.
```

#### Solution 2

Another approach is to do it all at once in the same function:

```haskell
f :: [Char] -> [[Char]]
f = filter (\s -> not (elem s ["a", "an", "the"])) . words
```

First split the string into individual words and then drop it if it is not "a", "an" or "the".
Function composition and point-free style were used for this soltuion.
