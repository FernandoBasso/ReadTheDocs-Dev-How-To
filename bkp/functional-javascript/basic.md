# Basic Functional Programming Tips

- [Basic Functional Programming Tips](#basic-functional-programming-tips)
  - [Useless Function Usage](#useless-function-usage)
  - [Another Useless Function Usage](#another-useless-function-usage)
  - [noLessThan10](#nolessthan10)
  - [noLessThan10 Point Free](#nolessthan10-point-free)

## Useless Function Usage

```js
const {
  forEach
} = require('ramda');

const log = console.log.bind(console);
```

You want to loop over a list of stuff, and log that stuff. People often do this:

```js
forEach(value => log(value), ['x', 'y', 'z']);
```

When they could simply have done:

```js
forEach(log, ['x', 'y', 'z']);
```

In the first example, we are _unnecessarily creating a new function for each element in the list_. With the second approach, we make the most of the fact that ECMAScript has Higher Order Functions, and we can simply pass a function that already exists and already does something we need, in this case, printing to the console. `log` is automatically passed the argument.

## Another Useless Function Usage

You want to double (multiply by two) the elements of a list of numbers.

```js
const notSoGod = map(num => double(num), [1, 2, 3]);
log(notSoGod);
// → [2, 4, 6]
```


_Don't_ create an anonymous function that takes an argument and calls `double` on that argument (as done above). It is _not_ optimal and not very idiomatic and elegant ECMAScript. Simply do this instead:

```js
const better = map(double, [1, 2, 3]);
log(better);
// → [2, 4, 6]
```

Again, the last version _does not_ create an unnecessary function for each element. We simply use the existing function directly. `map` takes care of passing the argument to `double`.


## noLessThan10

A function that produce its argument if it is more than 10, or always produce 10 if the argument is less than 10. Try to use `when`, `lt` and `__`.


```js
const { __, lt, when, always } = require('ramda');

/**
 * noLessThan10 :: Number -> Number
 */
const noLessThan10 = num =>
  when(lt(__, 10), always(10))(num);

log(noLessThan10(5));
// → 10
log(noLessThan10(11));
// → 11
```

## noLessThan10 Point Free

Now write `noLessThan10` using _Point Free_ style.

```js
const { __, lt, when, always } = require('ramda');

/**
 * Point free version.
 *
 * noLessThan10 :: Number -> Number
 */
const noLessThan10 = when(lt(__, 10), always(10)); // <1>

log(noLessThan10(5));
// → 10
log(noLessThan10(11));
// → 11
```

1. We _do not_ use the `num` parameter, like in the previous example.

