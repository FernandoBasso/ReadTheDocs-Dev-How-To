---
title: Cons, Data.List | PureScript
description: Ins and outs of PureScript `Cons' data constructor with practical examples.
---

# Cons (Data.List)

`Cons` is data constructor from `Data.List` `List` type.
It conses (short for *construct*, from Lisp parlance) a list by prepending an element to the *head* of a list.

It is not like `Array.prototype.push` in ECMAScript because it does not modify the existing structure, but returns a new list instead.

## Manually Create a List

```text
> import Data.List (List(Cons, Nil))

> Cons 1 Nil
(1 : Nil)

> Cons 1 (Cons 2 Nil)
(1 : 2 : Nil)

> Cons 1 (Cons 2 (Cons 3 Nil))
(1 : 2 : 3 : Nil)
```

Note that even though we use `Cons`, it prints in the REPL with `:`.

In PureScript, `Cons` is aliased to the `infixr` operator `:`. We use it like this:

```text
> import Data.List (List(Nil), (:)) 

> 1 : 2 : 3 : Nil
(1 : 2 : 3 : Nil)
```
