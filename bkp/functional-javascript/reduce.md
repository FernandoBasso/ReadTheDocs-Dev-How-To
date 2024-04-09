# Reduce

- [intro](#intro)
- [add](#add)
- [subtract](#subtract)
- [multiply](#multiply)
- [divide](#divide)

We always start with a shortcut for `console.log` and import some _rambda_ functions.

```js
const { reduce, add, subtract, multiply, divide } = require('ramda');
const log = console.log.bind(console);
```

## intro

`reduce` is a function that takes a function of two arguments, an accumulator, and a list of values. 

When invoked`reduce` passes the accumulator and the first element of the list to the function. That function then returns a new value for the accumulator, which is used again when `reduce` invokes the function passing the new accumulator value with the second element of the list, and so on until all the values of the list have been used once, upon which fact the value of the accumulator is returned.

## add

Ramda's `add` function takes exactly two arguments. If you pass more than two, they are simply ignored (that is how JS functions handle extra, unexpected arguments).

```js
log(add(1, 2, 3, 4));
// ➔ 3 (not 10. Args 3 and 4 are ignored)
```

But we can use `reduce` to help us out.

```js
const addResult = reduce(add, 0, [1, 2, 3, 4 ])
log(addResult);
// → 10
```

We pass `reduce` the `add` function, the initial value for the `sum`, and a list of numbers to add.

## subtract
The same with `subtract`. It takes precisely two arguments, ignoring any exceeding ones.

```js
log(subtract(1, 2, 3, 4));
// → -1 (3 and 4 are ignored)
```

`reduce` to the rescue.

```js
const subResult = reduce(subtract, 0, [1, 2, 3, 4]);
log(subResult);
// → -10 (as we expected)
```

## multiply
Same with `multiply`. It takes precisely two arguments. To multiply a list of values, we lay hand on `reduce` to help out.

```js
const multResult = reduce(multiply, 1, [1, 2, 3, 4]);
log(multResult);
// → 24
```

Note that with the combo “reduce/multiply”, we _must_ use `1` as the initial value. Basic maths :)

## divide

This is trickier. If we do `reduce(divide, 1, [5, 2])`, it does `1 / 5`, which is 0.2, then `0.2 / 2`, which is 0.1. It is very different than when adding, subtracting and multiplying, which we really only accumulate on an initial value. In the `divide` case, 

```js
const divResult = reduce(divide, 1, [5, 2]);
log(divResult);
// → 0.1
```

So, yeah, `divide` and division in general do not play well with `reduce`. In other words, division does not go well with reducing/fold functions.


