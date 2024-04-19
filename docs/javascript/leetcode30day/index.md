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


