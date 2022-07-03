---
title: Lists | PureScript
description: Some notes and important concepts and ideas a about lits in PureScript, including practical examples.
---

# Lists

Be aware that PureScript ships `Data.List` (`purescript-lists` package) and `Data.Array` (`purescript-arrays` package), and a few other list/array related packages.

In short, `Data.Array` leverages some low-level JavaScript array-related methods, while `Data.List` are really *strict* linked lists, allowing for pattern matching and cool recursive patterns.
Both have their pros and cons, and the choice depends on the task at hand.

## Basic Syntax for Creating Lists

Import the type `List` with its `Nil` and `Cons` data constructors, and create a simple list:

```text
> import Data.List (List(Nil, Cons))    
> Cons 1 (Cons 2 (Cons 3 Nil))
(1 : 2 : 3 : Nil)
```

Or import the `Cons` in its infixr operator form, `:`:

```text
> import Data.List (List(Nil), (:))
> 1 : 2 : 3 : Nil                  
(1 : 2 : 3 : Nil)
```

Note that we import `:` outside `List()`, unlike `Cons` earlier, even though `:` is infix operator alias for `Cons`.

One use of `Cons` is to create `Data.List` `singleton` function, which takes an element and creates a list with that one, single element:

```haskell
import Data.List (List(Nil, Cons))

singleton :: ∀ a. a -> List a
singleton e = Cons e Nil
```

Or using the infixr `:` operator instead of `Cons`:

```haskell
import Data.List (List(Nil), (:))

singleton :: ∀ a. a -> List a
singleton e = e : Nil
```

## References

- [Data.Array v7 docs on Pursuit](https://pursuit.purescript.org/packages/purescript-arrays/7.0.0/docs/Data.Array).
- [Data.List v7 docs on Pursuit](https://pursuit.purescript.org/packages/purescript-lists/7.0.0/docs/Data.List).
- [`:' infixr 6 binary cons operator for Data.List v7 docs on Pursuit](https://pursuit.purescript.org/packages/purescript-lists/7.0.0/docs/Data.List.Types#v:(:)).
- [In PureScript, how does List differ from Array? on Stack Overflow](https://stackoverflow.com/questions/44790037/in-purescript-how-does-list-differ-from-array).
