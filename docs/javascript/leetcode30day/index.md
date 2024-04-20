---
title: Leetcode 30 Days of JavaScript
---

# LeetCode 30 Days of JavaScript

## Introduction

- [30 Days of JavaScript study plan on LeetCode](https://leetcode.com/studyplan/30-days-of-javascript/).

Check the full source code, **including unit tests** in the [Gitlab repository for this project](https://gitlab.com/devhowto/dev-how-to/-/tree/main/src/javascript/leetcode30day/src).

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

### Apply Transform Over Each Element in Array (map)

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

### Filter Elements from Array (filter)

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
