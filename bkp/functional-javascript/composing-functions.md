# Composing Functions

- [eligible to vote - non-functional](#eligible-to-vote-non-functional)
- [eligible to vote - functional style](#eligible-to-vote-functional-style)
- [handling calculations with pipe and compose](#handling-calculations-with-pipe-and-compose)
    + [pipe](#pipe)
    + [compose](#compose)


## eligible to vote - non-functional

This example uses a more procedural, traditional code style.

```js
const log = console.log.bind(console);

const COUNTRY = 'JP';

const laraCroft = {
    name: 'Lara Croft',
    birthCountry: 'UK',
    naturalizationDate: '1996-01-01',
    age: 23,
    skill: 'Archaeology',
};

const wasBornInCountry = ({ birthCountry }) => birthCountry === COUNTRY;

const wasNaturalized = ({ naturalizationDate }) => Boolean(naturalizationDate);

const isOver18 = ({ age }) => age >= 18;

const isCitizen = person => wasBornInCountry(person) || wasNaturalized(person);

const isEligibleToVote = person => isOver18(person) && isCitizen(person);

log(isCitizen(laraCroft));
// → true
log(isEligibleToVote(laraCroft));
// → true
log(isEligibleToVote({ ...laraCroft, naturalizationDate: undefined }));
// → false
log(isEligibleToVote({ ...laraCroft, age: 17 }));
// → false
```

## eligible to vote - functional style

Using some ramda functions, it could be rewriten as this:

```js
const { either, both } = require('ramda');

const log = console.log.bind(console);

const COUNTRY = 'JP';

const laraCroft = {
    name: 'Lara Croft',
    birthCountry: 'UK',
    naturalizationDate: '1996-01-01',
    age: 23,
    skill: 'Archaeology',
};

// These three functions remain the same.
const wasBornInCountry = ({ birthCountry }) => birthCountry === COUNTRY;

const wasNaturalized = ({ naturalizationDate }) => Boolean(naturalizationDate);

const isOver18 = ({ age }) => age >= 18;

// Uses point-free style.
const isCitizen = either(wasBornInCountry, wasNaturalized); // <1>

// Uses point-free style.
const isEligibleToVote = both(isOver18, isCitizen); // <2>

log(isCitizen(laraCroft));
// → true
log(isEligibleToVote(laraCroft));
// → true
log(isEligibleToVote({ ...laraCroft, naturalizationDate: undefined }));
// → false
log(isEligibleToVote({ ...laraCroft, age: 17 }));
// → false
```

1. Using _point-free_ style makes it _not_ clear that `isCitizen` takes a `person`.
2. Idem.

Here are the two non-point-free versions of `isCitizen` and `isEligibleToVote`:

```js
// isCitizen :: Person -> Boolean
const isCitizen = person => either(wasBornInCountry, wasNaturalized)(person);

// isEligibleToVote :: Person -> Boolean
const isEligibleToVote = person => both(isOver18, isCitizen)(person);
```

## handling calculations with pipe and compose

We want to multiply a number by 2, then add 10, and then increment it. We can do:

```js
const { inc, add, multiply } = require('ramda');

const performOps = (num, mult) => {
  const product = multiply(num, mult);
  const sum = add(product, 10);
  const incremented = inc(sum);
  return incremented;
};

log(performOps(5, 3)); // (((5 * 3) + 10) + 1)
// → 26
```

We can do a bit better and use this approach instead.

```js
log(inc(add(multiply(5, 3), 10)));
// → 26
```

### pipe

And probably the most elegant version is this, which doesn't require a lot of saving results around (like in the first approach) and doesn't require an unwindly amount of nesting (like the second approach).

```js
const { pipe, inc, add, multiply } = require('ramda');
const performOps = pipe(multiply, add(10), inc);
log(performOps(5, 3));
// → 26
```

`add` takes two args. Since most (or all?) ramda functions are automatically curried, we pass `10` to `add`, and the `pipe` composition takes care of passing the other argument, result of `multiply`.

Once more, there is one downside that it is not clear the args `performOps2` requires. For that, one needs to look into the leftmost function inside the pipe.

Ever wondered why the [docs on ramda pipe](https://ramdajs.com/docs/#pipe) says:

> The leftmost function may have any arity; the remaining functions must be unary.

The leftmost one takes any number of arguments, and produces _a single value_ which is then passed to the next function, which _again produces a single value_. Since functions produce a single value, there is not way to produce multiple values/arguments to the next functions in the chain. That is why only the leftmost function may have any arity, because we pass the initial values _manually_, whereas the rest of the passing arguments is done by feeding return value of one function into another.

With the above explanation, it is also clear now that `add`, being passed only one argument (`10` in our example), returns a function that in turn takes _one_ argument. With that, _we satisfy `pipe`'s requirement that all functions except the leftmost one must have an arity of one_.


### compose

`compose` works like `pipe`, except it starts evaluating from right to left, so, we have to invert the orders of the arguments.

```js
const { compose, inc, add, multiply } = require('ramda');
const performOps = compose(inc, add(10), multiply);
log(performOps(5, 3));
// → 26
```
