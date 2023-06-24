---
title: Vowel Count :: 7kyu Codewars Challenge
description: Some solutions and explanations on Vowel Count 7kyu Codewars challenge using different approaches and programming languages.
---

# Vowel Count

- [Vowel Count 7kyu Codewars challenge](https://www.codewars.com/kata/54ff3102c1bad923760001f3)

## Example Input

```
vowelCount ""
//=> 0
// No chars, zero vowels.

vowelCount "xyz"
//=> 0
// No vowels in the string.

-- All 5 chars are vowels.
vowelCount "aeiou"
//=> 5
// All chars are vowels.

vowelCount "yaeiou"
//=> 5
// “y” is not considered a vowel in this challenge.

vowelCount "bcdfghjklmnpqrstvwxyz"
//=> 0
// Only consonant chars on the string.
```

## Haskell

### v1

```haskell
isVowel :: Char -> Bool
isVowel c = elem c "aeiou"

incIf :: (a -> Bool) -> a -> Int -> Int
incIf f v n = case (f v) of
  True -> (+) n 1
  _    -> n

--
-- T.C: O(n)
-- S.C: O(1)
--
vowelCount :: [Char] -> Int
vowelCount str = go str 0
  where
    go :: [Char] -> Int -> Int
    go [] cnt           = cnt
    go (chr : []) cnt   = go [] (incIf isVowel chr cnt)
    go (chr : chrs) cnt = go chrs (incIf isVowel chr cnt)
```

Here the “go function” pattern is used.
It uses pattern matching for the empty string and the string with a single char, or a string with a char and “more string”.

`incIf` takes a predicate function and returns a boolean.
It uses the predicate to decide if it should increment the `cnt` accumulator or not.

The time complexity is $O(n)$ as we iterate over the entire input once.
The iteration of the vowels in `isVowel` is of little concern as it is a short string, with known length so we simplify to $O(1)$.

The space complexity is $O(1)$ as our function doesn't need extra storage besides a numeric variable accumulator.

- [Discussion on Haskell Discord](https://discord.com/channels/280033776820813825/505367988166197268/1122267086400540722).
