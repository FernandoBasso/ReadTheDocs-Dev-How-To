# Folding Lists - Chapter 10

- [Folding Lists - Chapter 10](#folding-lists-chapter-10)
    - [01](#anchor-01)
    - [02](#anchor-02)
    - [03](#anchor-03)
    - [04](#anchor-04)
    - [05](#anchor-05)
      - [a](#a)
      - [b](#b)
      - [c](#c)
      - [d](#d)
      - [e](#e)
      - [f](#f)
      - [g](#g)
      - [h](#h)
      - [i](#i)
  - [Exercises: Database processing](#exercises-database-processing)
  - [Scans exercises](#scans-exercises)
  - [Warm-up and review](#warm-up-and-review)
    - [01](#anchor-01-1)
    - [02](#anchor-02-1)
    - [03](#anchor-03-1)
    - [Old Versions](#old-versions)
    - [The End](#the-end)

Page 365.

### 01

B and C are correct. Both `foldr` and `foldl` will do because `(*)` is associative. Flipping `(*)` doesn’t change the results, again, because `(*)` is associative.

### 02

```
foldl * 1 [1, 2, 3]

foldl (flip (*)) 1 [1, 2, 3]

foldl (flip (*)) (flip (*) 1 1) [2, 3]

foldl (flip (*)) (flip (*) 1 1) [2, 3]

foldl (flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) [3]

foldl (flip (*)) ((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3) []

((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3)

((flip (*)) ((flip (*)) 1 2) 3)

((flip (*)) 2 3

6
```

Another approach:

```
foldl :: (acc -> x -> acc) -> acc -> [x] -> acc
foldl f z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

f = (*)

foldl (flip f) ((flip f) 1 1) [2, 3]
acc = 1, (1 * 1)

foldl (flip f) ((flip f) 1 2) [3]
acc = 2, (1 * 2)

foldl (flip f) ((flip f) 2 3) []
acc = 6, (2 * 3)

Reaches base case, returns ‘z’, our ‘acc’.
```

Or, assigning `flip (*)` to a simpler variable to reduce number of parentheses:

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

### 03

C is correct.

### 04

A is correct.

### 05

#### a

Missing the “zero”, default, accumulator parameter.

Correct:

```ghci
λ> foldr (++) [] ["woot", "WOOT", "woot"]
"wootWOOTwoot"
```

#### b

Need a list of strings, not a single string.

Correct:

```ghci
λ> foldr max [] ["fear", "is", "little", "death"]
"little"
```

#### c

There is no “and” function.

Correct:

```haskell
foldr (&&) True [False, True]
```

#### d

It always returns `True`, which is incorrect. We must make the zero produce `False` instead.

```haskell
foldr (||) False [False, True]
```

#### e

```ghci
λ> foldr ((++) . show) "" [1..5]
"12345"
```

Or flipping `((++) . show)`, although the result is in reverse order:

```GHCi
λ> foldl (flip ((++) . show)) "" [1, 2, 3]
"321"
```

`foldr` applies `f x` first, so we apply `((++) . show)`to `1` first, converting it to a string, which is `(++)` concatenated to the “rest of the fold”.

`foldl` applies `f` to the zero/acc first, which *is* a string already, and then tries to `(++)`concatenate it with the number which was *not* converted to a string. It blows up! 💣

```
λ> ((++) . show) 1 ""
"1"

λ> ((++) . show) "" 1
💥 error
```

#### f

Zero has the type ‘Char’, and the list is of type ‘Num’. The type of zero and the elements of the list must be the same. Possible solutions, depending on the result sought:

```ghci
λ> foldr (flip const) 'a' [1, 2, 3]
'a'

λ> foldl const 'a' [1, 2, 3]
'a'

λ> foldr const 'a' "bcd"
'b'

λ> foldr const 'a' ['b', 'c', 'd']
'b'

λ> foldr const 0 [1, 2, 3]
1
```

#### g

`foldr const 0 "tacos"` is incorrect because the accumulator char `'t'` does not match the type of 0 (the number zero). Possible solutions depending on the result sought:

```ghci
λ> foldl const 0 "tacos"
0

λ> foldr (flip const) 0 "tacos"
0

λ> foldl const "" "tacos"
```

#### h

```ghci
λ> foldr (flip const) 0 "burritos"
0

λ> foldl const 0 "burritos"
0
```

#### i

```ghci
λ> foldr (flip const) 'z' [1..5]
'z'

λ> foldl const 'z' [1..5]
'z'
```

## Exercises: Database processing

Page 371.

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.Pretty.Simple (pPrint)
import Data.Time

pp = pPrint

data DBItem = DBString String
            | DBNumber Integer
            | DBDate UTCTime
            deriving (Eq, Ord, Show)
theDb :: [] DBItem
theDb =
  [ DBDate (UTCTime (fromGregorian 1911 5 1)
                    (secondsToDiffTime 34123))
  , DBNumber 9001
  , DBString "Hello, World!"
  , DBDate (UTCTime (fromGregorian 1921 5 1)
                    (secondsToDiffTime 34123))
  , DBNumber 100
 ]

filterDbDate :: [] DBItem -> [] UTCTime
filterDbDate items = foldr f [] items
  where
    --   (a     -> b           -> b)
    f :: DBItem -> [] UTCTime -> [] UTCTime
    f (DBDate x) acc = (:) x acc
    f _          acc = acc
--
-- λ> pp (filterDbDate theDb)
-- [ 1911 - 05 - 01 09 : 28 : 43 UTC
-- , 1921 - 05 - 01 09 : 28 : 43 UTC
-- ]
--
-- `foldr` passes one `DBItem` at a time to the folding function `f`.
--

filterDbNumber :: [] DBItem -> [] Integer
filterDbNumber items = foldr f [] items
  where
    --   (a     -> b           -> b)
    f :: DBItem -> [] Integer -> [] Integer
    f (DBNumber n) nums = (:) n nums
    f _            nums = nums
--
-- λ> pp $ filterDbNumber theDb
-- [ 9001 ]
--
-- `foldr` passes one `DBItem` at a time to the folding function `f`.
--
-- So, we can't return a `[] DBNumber`. We use it to pattern match on the
-- item, but since `DBNumber` is a data constructor (and not a type), we can't
-- use it in type signatures. Pattern matching is for runtime data.
--


mostRecent :: [] DBItem -> UTCTime
mostRecent items = maximum $ filterDbDate items

leastRecent :: [] DBItem -> UTCTime
leastRecent items = minimum $ filterDbDate items

sumDbNumbers :: [DBItem] -> Integer
sumDbNumbers = sum . filterDbNumber

avgDbNums :: [DBItem] -> Double
avgDbNums items = total / count
  where
    count :: Double
    count = fromIntegral . length . filterDbNumber $ items
    total :: Double
    total = fromIntegral . sumDbNumbers $ items
```

## Scans exercises

Page 378.

```haskell
myScanl :: (a -> b -> a) -> a -> [b] -> [a]
myScanl f q ls =
  q : (case ls of
         [] -> []
         x:xs -> myScanl f (f q x) xs)


res1 = myScanl (+) 1 [1..3]


fib :: Word -> Word
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


fibs :: [Word]
fibs = 1 : myScanl (+) 1 fibs


fibsN :: Int -> Word
fibsN n = fibs !! n


fibs20first :: [Word]
fibs20first = take 20 fibs

fibs20 :: [Word]
fibs20 = take 20 $ 1 : myScanl (+) 1 fibs20

fibsLT100 :: [Word]
fibsLT100 = takeWhile (< 100) fibs

fact :: Word -> Word
fact 0 = 1
fact n = n * fact (n - 1)


facts :: [Word]
facts = myScanl (*) 1 [1..]

factN :: Int -> Word
factN n = facts !! n
```

## Warm-up and review

Page 378.

### 01

```haskell
stops :: [Char]
stops = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

-- Produces all possible three-tuples of stop-vowel-stop combinations.
allCombs :: [Char] -> [Char] -> [(Char, Char, Char)]
allCombs ss vs = [(s, v, s) | s <- ss, v <- vs]

-- Checks whether a tuple starts with the given letter.
startsWith :: Char -> (Char, Char, Char) -> Bool
startsWith c (e, _, _) = c == e

startsWithAorP :: (Char, Char, Char) -> Bool
startsWithAorP (x, _, _) = x == 'a' || x == 'p'

nouns :: [[Char]]
nouns = ["jedi", "padawan", "kitten"]

verbs :: [[Char]]
verbs = ["fight", "run", "meow"]

mkNounVerbNoun :: [[Char]] -> [[Char]] -> [([Char], [Char], [Char])]
mkNounVerbNoun ns vs = [(n, v, n) | n <- ns, v <- vs]
```

### 02

Finds the average of the length of the words in the input string.

### 03

```haskell
-- Using ‘where’.
avgWordLen :: [Char] -> Double
avgWordLen str = (/) numWordChars lenWords
  where
    numWordChars = fromIntegral $ sum (map length (words str))
    lenWords     = fromIntegral $ length (words str)

-- Using ‘let’.
avgWordLen :: [Char] -> Double
avgWordLen str =
  let
    numWordChars = fromIntegral $ sum (map length (words str))
    lenWords     = fromIntegral $ length (words str)
  in
    (/) numWordChars lenWords

--
-- Version I come up during 2021 studies of this book.
--
avgWordLen :: String -> Double
avgWordLen s = (/) numChars numWords
  where
    numChars :: Double
    numChars = fromIntegral $ sum $ map length $ words s
    numWords :: Double
    numWords = fromIntegral $ length $ words s
--
-- λ> avgWordLen "The force is strong with this one. Most impressive!"
-- 4.777777777777778
--
```

----
### Old Versions

```haskell
-- Direct recursion, not using ‘&&’.
myAnd' :: [Bool] -> Bool
myAnd' [] = True
myAnd' (x:xs) =
  if x == False
  then False
  else myAnd xs

-- Direct recursion using ‘&&’.
myAnd'' :: [Bool] -> Bool
myAnd'' [] = True
myAnd'' (x:xs) = x && myAnd' xs

-- Fold, not point-free.
myAnd''' :: [Bool] -> Bool
myAnd''' = foldr f True
  where f = (\x acc ->
               if x == False
               then False
               else acc)

-- Both ‘myAnd’ and the folding function are point-free.
myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True


-- Direct recursion not using ‘||’.
myOr' :: [Bool] -> Bool
myOr' [] = False
myOr' (x:xs) =
  if x == True
  then True
  else myOr' xs

-- Direct recursion using ‘||’.
myOr'' :: [Bool] -> Bool
myOr'' [] = False
myOr'' (x:xs) = x || myOr'' xs

-- Fold, not point-free.
myOr''' :: [Bool] -> Bool
myOr''' = foldr f False
  where f = (\x acc ->
               if x == True
               then True
               else acc)

-- Both ‘myOr’ and the folding function are point-free.
myOr :: [Bool] -> Bool
myOr = foldr (||) False


-- Direct recursion not using ‘||’.
myAny' :: (a -> Bool) -> [a] -> Bool
myAny' _ []     = False
myAny' f (x:xs) =
  if f x == True
  then True
  else myAny' f xs

-- Direct recursion using ‘||’.
myAny'' :: (a -> Bool) -> [a] -> Bool
myAny'' _ []     = False
myAny'' f (x:xs) = f x || myAny' f xs

-- Fold, not point-free.
myAny''' :: (a -> Bool) -> [a] -> Bool
myAny''' f = foldr g False
  where g = (\x acc ->
               if f x == True
               then True
               else acc)


myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (||) False . map f



myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e xs = myAny (== e) xs


myEl :: Eq a => a -> [a] -> Bool
myEl e = foldr (||) False . map (== e)

myEl' :: Eq a => a -> [a] -> Bool
myEl' e = foldr (\x acc -> acc || x == e) False


myRev :: [a] -> [a]
myRev = foldr (\x acc -> acc ++ [x]) []

myRev' :: [a] -> [a]
myRev' = foldl (flip (:)) []


myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc ->
                      case f x of
                        True -> x : acc
                        False -> acc) []

squinsh :: [[a]] -> [a]
squinsh = foldr (\xs acc -> xs ++ acc) []


-- Why ‘(a -> [b])’ instead of ‘(a -> b)’?
squinshMap :: (a -> [b]) -> [a] -> [b]
squinshMap f = foldr (\x acc -> f x ++ acc) []
--
-- λ> squinshMap (flip (:) []) [1..3]
-- [1,2,3]
--

squinshMap' :: (a -> [b]) -> [a] -> [b]
squinshMap' f = foldr ((++) . f) []

squinshAgain :: [[a]] -> [a]
squinshAgain = squinshMap id


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

### The End
