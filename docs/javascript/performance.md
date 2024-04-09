---
title: Performance | JavaScript
description: Notes, tips, concepts, ideas and examples on JavaScript performance.
---

# Performance

## Copy Array: push() vs Spread

### Setup

For this case-study, assume we have these setup lines in our `.js` file:

```javascript
import { performance } from 'node:perf_hooks';

const log = console.log.bind(console);

function toInt(n) {
  return n | 0;
}
```

### Array of Random Numbers

We have a large array of numbers and want to make a copy of it.
Should we use spread, which is the “new awesome idiomatic way”, or the “laughable, old-school, n00b style using push()”?

First, create an array of 10000 random numbers:

```javascript
var nums = Array.apply(null, { length: 1e5 })
  .map(Function.call, Math.random);
```

### push()

Let's inspect the time it takes to copy the array using `Array.prototype.push()`:

```javascript
var pushIni = performance.now();

var numsCopy1 = nums.reduce((acc, n) => {
  acc.push(n);
  return acc;
}, []);

var pushEnd = performance.now();

log('PUSH:', toInt(pushEnd - pushIni));
```

Running the above “push()” snippet a bunch of times, it always took between 10 and 30 milliseconds on my current hardware.

### Spread

Now, let's inspect the time it takes to copy the array using spread:

```javascript
var spreadIni = performance.now();

var numsCopy2 = nums.reduce((acc, n) => {
  return [...acc, n];
}, []);

var spreadEnd = performance.now();

log('SPREAD:', toInt(spreadEnd - spreadIni));
```

Running the above “spread” snippet a bunch of times, it always took more than a minute on my current hardware.

### Conclusion

Spreads are incredibly more costly than some other approaches that mutate existing data structures.

In the `push()` example, we only ever add one more value each time to the existing array.

With the _spread_ approach, we always create a new array, copy the current existing values to it and add the new value.
And it does this each time (or each iteration of the `reduce`), with ever increasing number of values to copy.
Let me repeat: it copies and recopies an ever increasing number of values.

In hindsight, it is no surprise the the performance difference (time _and_ space complexity) is almost unbelievable higher with spreads.

In many places, copying data, returning copy of data in functions, or passing copy of data to functions will not be a noticeable problem for very small pieces of data, but it can quickly become a performance problem for larger pieces of data or if used everywhere without some consideration.

Some languages have more performant immutable data structures, but in ECMAScript, that is not the case.
ECMAScript does not have immutable data structures.
We sometimes make them immutable by applying some coding standards and principles (which are useful, sure), but that does not come cost-free.

Here's [the full code](https://gitlab.com/devhowto/dev-how-to/-/blob/devel/src/javascript/performance/perf_array_spread_push.js) just in case:

```javascript
//
// Make sure package.json contains "type": "module". Then,
// run the code with a command like this:
//
//   $ node --experimental-modules ./file.js
//

import { performance as perf } from 'node:perf_hooks';

const log = console.log.bind(console);

function toInt(n) {
  return n | 0;
}

////
// An array of 10000 numbers.
//
var nums = Array.apply(null, { length: 1e5 })
  .map(Function.call, Math.random);

////////////////////////////////////////////////////////////////////////
// push()
//
var pushIni = perf.now();

var numsCopy1 = nums.reduce((acc, n) => {
  acc.push(n);
  return acc;
}, []);

var pushEnd = perf.now();

log('PUSH:', toInt(pushEnd - pushIni));

////////////////////////////////////////////////////////////////////////
// spread
//
var spreadIni = perf.now();

var numsCopy2 = nums.reduce((acc, n) => {
  return [...acc, n];
}, []);

var spreadEnd = perf.now();

log('SPREAD:', toInt(spreadEnd - spreadIni));
```

## Other Resources

- [Cesium contributor coding guide with several tips on performance](https://github.com/CesiumGS/cesium/blob/main/Documentation/Contributors/CodingGuide/README.md)
