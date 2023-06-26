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

### v2

```haskell
isVowel :: Char -> Bool
isVowel = (`elem` "aeiou")

--
-- T.C: O(n).
-- S.C: O(1).
--
vowelCount :: [Char] -> Int
vowelCount = length . filter isVowel
```

Point-free style and function composition.
T.C. $O(1)$ because computing the result of `filter` and `length` is done on a single pass.

- [Discussion on one or two passes on Haskell Discord](https://discord.com/channels/280033776820813825/505367988166197268/1122492590844031127).

### v3

```haskell
isVowel :: Char -> Bool
isVowel = (`elem` "aeiou")

--
-- T.C: O(n).
-- S.C: O(1).
--
vowelCount :: [Char] -> Int
vowelCount str = length [c | c <- str, isVowel c]
```

Using list comprenhesions.

## JavaScript

### v1

```javascript
function isVowel(chr) {
  return "aeiou".includes(chr);
}

/**
 * T.C: O(n).
 * S.C: O(n).
 *
 * @param {string} str
 * @returns {number}
 */
function vowelCount(str) {
  return [...str].reduce(function count(memo, chr) {
    return isVowel(chr) ? memo + 1 : memo;
  }, 0);
}
```

Time complexity is $O(n)$ because we reduce (iterate once) to compute the count (through the `memo` reducer param).
Even though we spread the string (`[...str]`) which is another form of loop happening behind the scenes (which means we have $O(n * 2)$), we do not have nested loops some T.C. is simplified to $O(n)$.

As for the space complexity, we spread the string, thus creating a copy of it, it has S.C. $O(n)$.

### v2

```javascript
/**
 * T.C: O(n).
 * S.C: O(1).
 *
 * @param {string} str
 * @returns {number}
 */
function vowelCount(str) {
  let count = 0;

  for (let c of str)
    if (isVowel(c)) ++count;

  return count;
}
```

We use the same `isVowel()` from the previous solution.

Instead of spreading and then reducing, we simply iterate over each char of the input string.
With reduce, we need a callback function.
With the simple for/of, we don't even need that.

The T.C. is $O(n)$ as we iterate once (really only once, unlike the previous example).

The S.C. is $O(1)$ this time as we don't make a copy of the string as in the previous example.

### v3

```javascript
const lookup = {
  a: true,
  e: true,
  i: true,
  o: true,
  u: true,
};

/**
 * T.C: O(n).
 * S.C: O(1).
 *
 * @param {string} str
 * @returns {number}
 */
function vowelCount(str) {
  let count = 0;

  for (let c of str)
    if (lookup[c]) ++count;

  return count;
}
```

This time we use a simple lookup table, which allows us not to have to do the loop implicit by `includes()` in the previously used `isVowel()` helper function.

Because objects allow for constant time $O(1)$ access in JavaScript, this is a nice approach too.
It doesn't cause the `if` condition to have to do a loop to check for the “vowelness” of the current character of the iteration.

Also, we could have used `1` instead of `true` for the lookup table values, but then the engine would have to coerce them to bools before actually performing the check.

The T.C. of this solution is $O(n)$, but in reality, is is likely to be the more performant of the JS solutions shown here so far.

The S.C. is $O(1)$. The small lookup table is small with a known length of 5, so it is considered constant space complexity.
