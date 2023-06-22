# Valid Parenthesis :: 7kyu Codewars Challenge

- [Valid Parenthesis 7kyu Codewars challenge^](https://www.codewars.com/kata/6411b91a5e71b915d237332d)

In the comments, people say this problem should bin in the 6kyu category.

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
