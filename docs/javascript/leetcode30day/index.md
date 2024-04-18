---
title: Leetcode 30 Days of JavaScript
---

# LeetCode 30 Days of JavaScript

## Introduction

- [30 Days of JavaScript study plan on LeetCode](https://leetcode.com/studyplan/30-days-of-javascript/).

Check the full source code, **including unit tests** in the [Gitlab repository for this project](https://gitlab.com/devhowto/dev-how-to/-/tree/main/src/javascript/leetcode30day/src).


## 01 Hello

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

## 02 Counter

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

## 03 To Be Or Not To Be (Expect)

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

```{toctree}
---
maxdepth: 6
caption: LeetCode 30 Days of JavaScript
---
```
