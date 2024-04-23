---
title: Leetcode 30 Days of JavaScript
---

# LeetCode 30 Days of JavaScript

## Introduction

- [30 Days of JavaScript study plan on LeetCode](https://leetcode.com/studyplan/30-days-of-javascript/).

Check the full source code, **including unit tests** in the [Gitlab repository for this project](https://gitlab.com/devhowto/dev-how-to/-/tree/main/src/javascript/leetcode30day/src).

## Performance

In general, using some new ECMAScript features will result in less performant results (both in terms of execution time and memory footprint).

For example, using [...spread is orders of magnitude more computationally costly](../performance.md) than good old `Array.prototype.push()` to add elements to, or copying an array.

Similarly, recursion, call stack and the like are more costly (in JavaScript) than some good old for loop approaches.

Using helper functions, immutable data structures and the best coding practices regarding readability and elegance don't always result in the most performant code.

That is why some of the problems are implemented in a few different ways.
Some approach to make it more performant, and some other approaches to use a more functional programming style, immuatable data structures, or some other thing we might find fun and/or useful to explore.

## Closures

### Hello

- [Create Hello World Function on LeetCode](https://leetcode.com/problems/create-hello-world-function/description/?envType=study-plan-v2&envId=30-days-of-javascript).

```javascript
/**
 * @returns {Function}
 */
function createHelloWorld() {
  return function hello() {
    return "Hello World";
  };
}
```

### Counter

- [Counter on LeetCode](https://leetcode.com/problems/counter/description/?envType=study-plan-v2&envId=30-days-of-javascript).

```javascript
/**
 * @param {number} n
 * @returns {() => number}
 */
function createCounter(n) {
  var x = n;

  return function counter() {
    return x++;
  };
}
```

### To Be Or Not To Be (Expect)

- [Expect on LeetCode](https://leetcode.com/problems/to-be-or-not-to-be/description/?envType=study-plan-v2&envId=30-days-of-javascript)

```javascript
/**
 * @param {unknown} actual
 * @returns {{
 *  toBe: (expected: unknown) => true | never;
 *  notToBe: (expected: unknown) => true | never;
 * }}
 */
function expect(actual) {
  return {
    /**
     * Checks whether `actual === expected`.
     *
     * @param {unknown} expected
     * @throws {Error}
     * @returns {true}
     */
    toBe: function toBe(expected) {
      if (expected !== actual)
        throw Error("Not Equal");

      return true;
    },

    /**
     * Checks whether `actual !== expected`.
     *
     * @param {unknown} expected
     * @throws {Error}
     * @returns {true}
     */
    notToBe: function notToBe(expected) {
      if (expected === actual)
        throw Error("Equal");

      return true;
    },
  };
}
```

### Counter II

- [Counter II on LeetCode](https://leetcode.com/problems/counter-ii/?envType=study-plan-v2&envId=30-days-of-javascript).

```javascript
/**
 * Returns an object with with a few counter-related methods.
 *
 * - T.C: O(1).
 * - S.C: O(1).
 *
 * @param {number} init The initial integer value for the counter.
 * @returns {{
 *   increment: () => number;
 *   decrement: () => number;
 *   reset: () => number;
 * }}
 */
function createCounter(init) {
  var count = init;

  return {
    /**
     * - T.C: O(1).
     * - S.C: O(1).
     *
     * @returns {number}
     */
    increment: function increment() {
      return ++count;
    },

    /**
     * - T.C: O(1).
     * - S.C: O(1).
     *
     * @returns {number}
     */
    decrement: function decrement() {
      return --count;
    },

    /**
     * - T.C: O(1).
     * - S.C: O(1).
     *
     * @returns {number}
     */
    reset: function reset() {
      return count = init;
    },
  };
}
```

In `reset()`, we are doing `return count = init`.
We could add parenthesis around the assignment, like `(count = init)`, but the assignment has higher precedence anyway and will happen **before** the value is returned.

## Basic Array Transformations

### Apply Transform Over Each Element in Array (map())

- [Apply Transform Over Each Element in Array on LeetCode](https://leetcode.com/problems/apply-transform-over-each-element-in-array/?envType=study-plan-v2&envId=30-days-of-javascript).

```javascript
/**
 * Applies a function to each element of the input array and returns
 * a new array with the result.
 *
 * - T.C: O(n). Applies fn once per each element. The final time
 *   complexity will depend on the time complexity of the callback.
 * - S.C: Same notes as for T.C.
 *
 * @param {unknown[]} xs
 * @param {(val: number, idx: number) => unknown} fn The index of
 *   the current element is passed to the callback, but it is up
 *   to the callback to decide if it wants to use it or not.
 * @returns {unknown[]}
 */
function map(xs, fn) {
  var mapped = [],
      x,
      i;

  for (i = 0; x = xs[i], i < xs.length; ++i)
    mapped.push(fn(x, i));

  return mapped;
}
```

Using [...spread syntax is way costly and slower](../performance.md) than good old `Array.prototype.push()`.

Also, this implementation does not mutate the input array and therefore using `push()` is not really bad at all in this case.
Only the new array which is returned is mutated while it is being constructed, but from the client code point of view, this `map()` implementation is pure.

### Filter Elements from Array (filter())

- [Filter Elements from Array on LeetCode](https://leetcode.com/problems/filter-elements-from-array/?envType=study-plan-v2&envId=30-days-of-javascript).

```javascript
/**
 * Returns a new array containing only elements from the original
 * array that satisfy the predicate.
 *
 * - T.C: O(n). Applies `predicate` once per each element. The final
 *   time complexity will depend on the time complexity of the
 *   predicate.
 * - S.C: Same notes as for T.C.
 *
 * @param {unknown[]} xs
 * @param {(val: number, idx: number) => unknown[]} fn The index of
 *   the current element is passed to the predicate, but it is up
 *   to the predicate to decide if it wants to use it or not.
 * @returns {unknown[]}
 */
function filter(xs, predicate) {
  var filtered = [],
    x,
    i;

  for (i = 0; x = xs[i], i < xs.length; ++i)
    if (predicate(x, i))
      filtered.push(x);

  return filtered;
}
```

Same notes for `map()` apply here regarding the use of `push()` and a simple loop.

### Array Reduce Transformation (reduce())

- [Array Reduce Transformation on LeetCode](https://leetcode.com/problems/array-reduce-transformation/?envType=study-plan-v2&envId=30-days-of-javascript).

#### Using Good Old for Loop

```javascript
/**
 * Applies a reducing function to `xs` and returns the result.
 * Returns `init` if `xs` is empty.
 *
 * - T.C: O(n), but final T.C will depend on T.C of reducing fn.
 * - S.C: Same notes as T.C.
 *
 * @param {unknown[]} xs
 * @param {(acc: unknown, x: unknown) => unknown} fn
 * @param {unknown} init
 * @returns {unknown}
 */
function reduce(xs, fn, init) {
  var { length: len } = xs,
      acc = init,
      x,
      i;

  for (i = 0; x = xs[i], i < len; ++i)
    acc = fn(acc, x);

  return acc;
}
```

#### Using Recursion

```javascript
const head = xs => xs[0];
const tail = xs => xs.slice(1);
const isEmpty = xs => xs.length === 0;

/**
 * Applies a reducing function to `xs` and returns the result.
 * Returns `init` if `xs` is empty.
 *
 * - T.C: O(n), but final T.C will depend on T.C of reducing fn.
 * - S.C: Same notes as T.C.
 *
 * @param {unknown[]} xs
 * @param {(acc: unknown, x: unknown) => unknown} fn
 * @param {unknown} init
 * @returns {unknown}
 */
function reduce(xs, fn, init) {
  return (function go(acc, elems) {
    return isEmpty(elems)
      ? acc
      : go(fn(acc, head(elems)), tail(elems));
  }(init, xs));
}

export { reduce };
```

### Function Composition (compose())

- [Function Composition on LeetCode](https://leetcode.com/problems/function-composition/description/?envType=study-plan-v2&envId=30-days-of-javascript).

#### for loop

```javascript
/**
 * Applies all functions from left to right. Works as the identity
 * function if `fns` is empty.
 *
 * - T.C: Depends on T.C of input `fns` and `val`.
 * - S.C: Same notes as T.C apply.
 *
 * @param {Array<Function>} fns
 * @returns {(val: unknown) => unknown}
 */
function compose(fns) {
  /**
   * @param {unknown} val
   * @returns {unknown}
   */
  return function composed(val) {
    var lastFnIdx = fns.length - 1,
        result = val,
        i;

    for (i = lastFnIdx; i >= 0; --i)
      result = fns[i](result);

    return result;
  };
}
```
