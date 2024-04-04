---
title: Arrays | JavaScript
description: Notes, tips and examples on using arrays in JavaScript.
---

# Arrays

## Array of n elements

```javascript
Array(5).fill(0);
```

The above makes a sparse array, and sparse arrays are permanently slow.

Using `Array()` has always been considered a bad practice and engines never bothered to optimizing it as they would have to check the prototype chain.

This is the more recommended way of creating an array of $n$ elements:

```javascript
Array.from({ length: 5 });
//=> [ undefined, undefined, undefined, undefined, undefined ]

Array.from({ length: 5 }).map((_, i) => i);
//=> [ 0, 1, 2, 3, 4 ]
```

## Return array with conditional item

Suppose we want to return an array like this:

```js
function getScripts() {
  return [
    "one.js",
    "two.js",
    "three.js",
  ];
}
```

But imagine based on some flag we want to skip "one.js" and return only the other two scripts.
One approach is using some spread trickery:

```javascript
function getScripts(b) {
  return [
    ...(b ? ["one.js"] : []),
    "two.js",
    "three.js",
  ];
}

getScripts(0);
//=> ["two.js", "three.js"]


getScripts(1);
//=> ["one.js", "two.js", "three.js"]
```

But the iterator protocol is slower than some alternatives.
Doing the spread is idiomatic enough, but if performance becomes a problem, using `[].concat()` is an alternative (and we want to avoid mutations like when using `unshift()` `push()`).

```javascript
function getScripts(b) {
  return [].concat(
    b
      ? ["one.js"]
      : [], "two.js", "three.jsx"
  );
}

getScripts(0);
//=> ["two.js", "three.js"]


getScripts(1);
//=> ["one.js", "two.js", "three.js"]
```
