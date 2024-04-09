# Haskell Operators Precedence

Function application has the highest precedence. 10 out of 10. Newer GHC versions say it has precedence of -1. Yet, the highest precedence.

Composition operator has a precedence of 9.

```
λ> :info (->)
type (->) :: * -> * -> *
data (->) a b
infixr -1 ->

λ> :info (.)
(.) :: (b -> c) -> (a -> b) -> a -> c
infixr 9 .

λ> :info ($)
($) :: (a -> b) -> a -> b
infixr 0 $
```



## References

* https://typeclasses.com/featured/dollar Amazing post about Haskell `$` infixr 0 operator. A MUST read. To be read many times.
* [https://wiki.haskell.org/$](https://wiki.haskell.org/$)

