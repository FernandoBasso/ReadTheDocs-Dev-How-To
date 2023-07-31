---
title: Simple Fun: Diplomas :: 7kyu Codewars Challenge
description: Solutions with notes and explanations for the Simple Fun: Diplomas 7kyu Codewars challenge in a few different programming languages
---

# Simple Fun: Diplomas

- [Simple Fun: Diplomas :: Codewars 7kyu Challenge](https://www.codewars.com/kata/591592b0f05d9a3019000087)

This is an example of a “rectangle packing” problem.

- [Rectangle Packing (Wikipedia)](https://en.wikipedia.org/wiki/Rectangle_packing#Packing_different_rectangles_in_a_minimum-area_rectangle)
- [What algorithm can be used for packing rectangles of different sizes into the smallest rectangle possible in a fairly optimal way? (Stack Overflow question)](https://stackoverflow.com/questions/1213394/what-algorithm-can-be-used-for-packing-rectangles-of-different-sizes-into-the-sm)

The code is simple enough, but knowing and understanding the maths behind it is not directly obvious, so I wouldn't say this is a 7kyu challenge.
It is harder than that.

## JavaScript

### Solution 1

```javascript
/**
 * Finds square size that can pack given number of rectangles.
 *
 * This is an example of a “packing rectangles” problem.
 *
 * @param {number} height
 * @param {number} width
 * @param {number} count
 * @return {number}
 */
function diplomas(height, width, count) {
  let _h, _w, side = 0;

  for (;;) {
    _h = Math.floor(side / height);
    _w = Math.floor(side / width);

    if (_h * _w >= count) return side;

    ++side;
  }
}
```

### Solution 2

```javascript
function floor(x) {
  return Math.floor(x);
}

/**
 * Finds square size that can pack given number of rectangles.
 *
 * This is an example of a “packing rectangles” problem.
 *
 * @param {number} h The height of each diploma.
 * @param {number} w The width of each diploma.
 * @param {number} c The number of diplomas.
 * @return {number}
 */
function diplomas(h, w, n) {
  if (n === 0) return 0;

  let side = 1;

  //
  // Like old-school C 😅.
  //
  while (1) {
    if (floor((side / h)) * floor((side / w)) >= n)
      return side;

    ++side;
  }
}
```
