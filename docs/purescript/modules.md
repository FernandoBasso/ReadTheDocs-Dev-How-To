---
title: Modules | PureScript
description: Some concepts and examples on how to create and use modules in PureScript.
---

# Modules

## Importing a type and its data constructors

**NOTE**: We are going to exemplify some of the syntax using `Data.List` module.

This is how we import `Data.List`'s `List` type, and its data constructors `Nil` and `Cons` (you may need to run `npx spago install lists` first):

```haskell
import Data.List (List(Nil, Cons))
```

Those are the two available data constructors for `Data.List`, according to `.spago/lists/v7.0.0/src/Data/List/Types.purs`:

```haskell
data List a = Nil | Cons a (List a)
```

So, yeah, `List` is the type  and `Nil` and `Cons` are the (only two) data constructors for the type `List`.


## SomeType(..) syntax

The `SomeType(..)` syntax is used to import the `SomeType` type with all its exported data constructors:

```haskell
import Data.List (List(..))
```

Since the type `List` (from `Data.List`) contains the data constructors `Nil` and `Cons`, both the `List` and the two data constructors `Nil` and `Cons`.

And to import the type, the data constructors for that type, and some more functions from that module, e.g.:

```haskell
import Data.List (List(..), (:)), tail, last)
```

Now we have the type `List`, the data constructors `Nil` and `Cons`, the infix operator `:`, and the functions `tail` and `last` from the module `Data.List` in scope.

```{admonition} Pay attention to the parentheses!!!
:class: info

Pay attention to the parentheses.
Note the parentheses around the type, and the inner parentheses around the data constructors.
Also, the parentheses around operators.
```

## References

- [Official PureScript docs on Modules](https://github.com/purescript/documentation/blob/master/language/Modules.md).

