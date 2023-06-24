---
title: Valid Parentheses :: 7kyu Codewars Challenge
description: Some solutions and explanations on Valid Parentheses 7kyu Codewars challenge using different approaches and programming languages.
----

# Valid Parentheses

## Intro

- [Valid Parentheses 7kyu Codewars challenge](https://www.codewars.com/kata/6411b91a5e71b915d237332d)

In the comments, people say this problem should bin in the 6kyu category.

Example input and output:

```
validParens ""                → True
validParens "("               → False
validParens "(("              → False
validParens ")(()))"          → False
validParens "()"              → True
validParens "()()"            → True
validParens "((()))"          → True
validParens "())()("          → False
validParens "(())((()())())"  → True
```

## Haskell

### v1

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}

incIf :: Char -> Char -> Int -> Int
incIf c1 c2 n
  | c1 == c2 = (+ 1) n
  | otherwise = n

validParens :: String -> Bool
validParens str = go str 0 0
  where
    go :: String -> Int -> Int -> Bool
    go s l r
      | length s == 0 = l == r
      | r > l = False
      | otherwise = go
                    (tail s)
                    (incIf (head s) '(' l)
                    (incIf (head s) ')' r)
```

### v2

Saw this example from another user's solution.
Not how I would do it, but I was curious on how it worked.

```haskell
validParens :: String -> Bool
validParens = (== 0) . foldr
              (\c n ->
                  if n < 0
                  then n
                  else
                    if c == ')'
                    then n + 1
                    else n - 1) 0
```

### v3

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}

incIf :: Char -> Char -> Int -> Int
incIf c1 c2 n
  | c1 == c2 = (+ 1) n
  | otherwise = n

--
-- In go, instead of checking if the length is 0 and then using head and
-- tail later, consider pattern matching instead (which also allows GHC
-- to warn you about missing patterns).
--
validParens :: String -> Bool
validParens str = go str 0 0
  where
    go :: String -> Int -> Int -> Bool
    go [] l r = l == r
    go (h : t) l r
      | r > l = False
      | otherwise = go
                    t
                    (incIf h '(' l)
                    (incIf h ')' r)
```

## JavaScript

### v1

```javascript
/**
 * Recursive solution using the run/go helper function approach.
 *
 * - T.C: O(n).
 * - S.C: O(n).
 *
 * @param {string}
 * @returns {number}
 */
function validParens(s) {
  return (function go(lst, l, r) {
    if (lst.length === 0) return l === r;

    if (lst[0] === '(') ++l;
    if (lst[0] === ')') ++r;

    if (r > l) return false;

    return go(lst.slice(1), l, r);
  })([...s], 0, 0);
}
```

### v2

```javascript
/**
 * @param {string}
 * @returns {number}
 */
function validParens(s) {
  return (function go(l, c) {
    if (l.length === 0) return c === 0;

    if (l[0] === '(') ++c;
    if (l[0] === ')') --c;

    if (c < 0) return false;

    return go(l.slice(1), c);
  })([...s], 0);
}
```
