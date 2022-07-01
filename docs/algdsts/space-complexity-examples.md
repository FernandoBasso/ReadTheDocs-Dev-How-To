---
title: Space Complexity Examples | Algorithms and Data Structures in TypeScript and JavaScript
description: Let's analyze several examples to understand how we figure out the space complexity of a algorithms.
---

# Space Complexity Examples

## Sum Array of Numbers

Consider:

```typescript
function sum(xs: number[]): number {
  let total = 0;

  for (let i = 0; i < xs.length; ++i)
    total += xs[i];

  return total;
}
```

This simple and humble algorithm takes space for `total` and `i`.
When `i` is incremented or `total` is added to, the space does not change.
There is time complexity increase, sure, but the space complexity remains the same.
Incrementing a numeric variable doesn't take *more* space.
Therefore, this algorithm's space complexity is O(1).

![Sum Array Space Complexity](./space-complexity-examples.assets/space-complexity-sum-array.png)

## Double Array of Numbers

```typescript
function doubleNums(xs: number[]): number[] {
  const doubled: number[] = [];

  for (let i = 0; i < xs.length; ++i)
    doubled[i] = xs[i] * 2;

  return doubled;
}

export { doubleNums };
```

The `doubled` array starts empty, but gets longer and longer directly in proportion to the length of the input, which is significant!
Incrementing `i`, however, is not significant for space complexity.
It will always hold a number, no matter how large that number is.
But the `doubled` array will keep storing more and more numbers.

![Double Array of Numbers Space Complexity](!./../space-complexity-examples.assets/space-complexity-double-array.png)
