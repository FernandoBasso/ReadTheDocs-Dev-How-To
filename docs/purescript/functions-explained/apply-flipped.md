---
title: applyFlipped, # | Functions Explained | PureScript
description: Concepts, implementation and examples on the use of `applyFlipped' (the `#' operator) in PureScript.
---

# applyFlipped, ‘#’

Type signature:

```haskell
applyFlipped :: ∀ a b. a -> (a -> b) -> b
```

Compare with `apply`:

```haskell
       apply :: ∀ a b. (a -> b) -> a -> b
applyFlipped :: ∀ a b. a -> (a -> b) -> b
```

Given the type signatures, it is clear that `applyFlipped` is really the flipped version of `apply`.
Under the light of that piece of insight, one way to implement `applyFlipped` is in terms of `apply`.

```haskell
applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply
```

By writing `applyFlipped` in terms of `apply`, we can make it point-free.

Or we can write it “manually”, without reusing `apply`:

```haskell
applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped x f = f x
```

This is how we would use it.
Let's compare with `apply`:

```text
> apply show 1
"1"

> applyFlipped 1 show
"1"
```

We apply the `show` function to the value 1, or, using the flipped version, we first provide the value, then the function.


```{admonition} apply f to x or apply x to f
:class: info

We have function application, not value application.
By definition, it is correct to say that “we apply a function to a value”, and not that “we apply a value to a function”.
Still, it is becoming more common to say or write both interchangeably.
See [my question about this on the PureScript Discord server](https://discord.com/channels/864614189094928394/865401680497737758/990236627035111434).
```

```text
> import Prelude (show, identity)

> apply show 1
"1"

> applyFlipped 1 show
"1"

> apply show (identity 1)
"1"

> applyFlipped (identity 1) show
"1"
```

## Using ‘#’ (number/hash/pound symbol) infixl applyFlipped operator

Whatever implementation we may chose for `applyFlipped`, we can write it as an *infixl* operator:


```haskell
applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixl 0 applyFlipped as #
```

Here too, as in `apply`, we need the precedence to be 0 (the lowest possible) so we first evaluate things on the left (*infixl*) and only then evalutate things on the right.

And some examples of its use (comparing with `$`/`apply`):

```text
> import Effect.Console (log)
> import Prelude (identity, show)

> log $ show $ identity 1
1

> identity 1 # show # log
1
```

The `#` operator makes it more readable (at last to some people).
In the book [Functional Programming Made Easier](https://discourse.purescript.org/t/new-purescript-book-functional-programming-made-easier/2390), Charles Scalfani shows an example comparing `$` and `#`:

```text
f x $ step3 $ step2 $ step1 x
f x $ step1 # step2 # step3 x
```

Indeed, `#` (`applyFlipped`) does help with readability and reasoning about code in some situations.

If you have ever used [Ramda](https://ramdajs.com/), `apply` is like `compose()`, while `applyFlipped` is like `pipe()`.

- [Ramda compose example](https://ramdajs.com/repl/#?pipe%28%0A%20%20identity%2C%0A%20%20add%281%29%2C%0A%20%20toString%2C%0A%29%280%29%3B%0A).
- [Ramda pipe example](https://ramdajs.com/repl/#?pipe%28%0A%20%20identity%2C%0A%20%20add%281%29%2C%0A%20%20toString%2C%0A%29%280%29%3B%0A).

