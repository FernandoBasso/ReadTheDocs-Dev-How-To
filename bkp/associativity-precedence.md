# Associativity and Precedence

## Understanding `$'

@WORKING

`f $ a = f a`  “Immediately this seems a bit pointless until we  remember that it’s defined as an infix operator with the lowest possible precedence.” (page 54 on HFFP book). Why does it seem pointless?

`f $ x` means it will drop the `$`, and just evaluate `f x`.

And

```ghci
λ> :type ($)
($) :: (a -> b) -> a -> b 
```

I can't clearly see how `f $ a = f a` is the same as `($) :: (a -> b) -> a -> b`.

### Explanation

Thanks to @dminuoso in #haskell-beginners for the wonderful explanation.

First, `($) :: (a -> b) -> a -> b` is the same as `($) :: (a -> b) -> (a -> b)`, and `($) = id`.

The reason that it might seem pointless, is that in order to apply `f` to `x`, you don't need any operator to do that. You'd just write `f x`. So it should beg the question of why write `f $ x` in the first place, when you could have written `f x` instead.

So, in `(2^) $ 2 + 2`, it will partially apply `2^`, sectioning it to  the right, which is the function `(a -> b`), and then, `($)` being  infixr will cause `2 + 2` to be evaluated before the final application  of the earlier partially applied function?



The evaluation order is not specified here.  

We can't say what will be evaluated first.

`f $ ....` is the same as `f (....)`.

So `($)` is just an operator trick to control precedence (how things associate!).

It's a trick because `($)` is not a magic primitive. It just uses the fact that when we define an operator, we can define whether its left or right associative, and how strong it associates.

So because `($)` uses the lowest precedence possible (0), anything else associates stronger.

so `f t $ b x` associates as `(f t) $ (b x)`

If had written `f t b x` instead, it would have associated as `((f t) b) x` instead  

Mentally you can think of `$` as "group everything from here until the end of the expression in parens"

It also has some other interesting impacts, it lets you avoid parens on the left hand side as well  

e.g.

`(f . g . h) (x y)` could be written as `f . g . h $ x y` instead



## $ doesn't work

@WORKING

Why do neither of these work?

```ghci
λ> 2 * 2 $ ^ 2
<interactive>:38:9: error: parse error on input ‘^’
```

```ghci
λ> 2 * 2 ^ $ 2
<interactive>:40:9: error:
    parse error on input ‘$’
    Perhaps you intended to use TemplateHaskell
```

```ghci
λ> 2 * 2 $ (^2)
<interactive>:39:1: error:
    • Non type-variable argument in the constraint: Num ((a -> a) -> t)
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall a t. (Num a, Num ((a -> a) -> t)) => t
```

`$` is `id`, but infixr 0.

```ghci
λ> f n = n + 1
λ> f 1
2
λ> f $ 1
2
```

This works:

```ghci
λ> (2^) 2 + 2
6
λ> (2^) (2 + 2)
16
λ> (2^) $ 2 + 2
16
```

Note that `(2^)` is a function. It awaits the next argument, which is 2 in the first line, and 4 on the other two lines.

## Don't try to apply a constant value to a function

`(2 + 2) (*1)` is not `4 * 1`. Since `(*1)` is a function, a sectioned function, GHCi will try to apply 4 to `(*1)`, as if 4 was a function, which it is not, thus an error.

`2 + 2 $ (*1)` is the same as `(2 + 2) (*1)`.



But `(*1) (2 + 2)` works, because it is applying `(*1)` to 4. Indeed, a applying a function to a value, not a value to a function like in the earlier attempt.

`(*1) $ 2 + 2` is the same as `(*1) (2 + 2)`.





