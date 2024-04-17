---
title: Leetcode 30 Days of JavaScript
---

# LeetCode 30 Days of JavaScript 

- [30 Days of JavaScript study plan on LeetCode](https://leetcode.com/studyplan/30-days-of-javascript/).


## 01 Hello

- [Create Hello World Function](https://leetcode.com/problems/create-hello-world-function/description/?envType=study-plan-v2&envId=30-days-of-javascript).

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

- [Counter](https://leetcode.com/problems/counter/description/?envType=study-plan-v2&envId=30-days-of-javascript).

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

```{toctree}
---
maxdepth: 6
caption: LeetCode 30 Days of JavaScript
---
```
