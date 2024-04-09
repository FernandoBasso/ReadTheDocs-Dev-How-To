---
title: Split Integer Into Even Parts | Algorithms
description: A problem where we have to split an integer into n given parts, as evenly as possible.
---

# Split Int Into Even Parts

tags: [math, integer, division, remainder, modulo]

Divide an unknown integer into a given number of even parts — or at least as even as they can be, and the parts are returned as an array.
The sum of the parts should be the original value, but each part should be an integer, and they should be as close as possible.

Examples:

```text
spitInt(10, 1)
→ [10]

split(10, 3)
→ [3, 3, 4]

split(20, 6)
→ [3, 3, 3, 3, 4, 4]
```

There is some math stuff behind this, and it involves integer division and remainders (modulo division).
Let's consider splitting 20 into 6 parts.

```text
20 % 6 = 2

(20 - 2) / 6
   18 / 6
     3
```

We then fill all six positions in the array with 3: `[3, 3, 3, 3, 3, 3]`.

The remainder is 2, which means the last two positions need to be worked some
more and cannot be simply left as 3, else the total sum is 18, not 20.

The first four positions are OK, though, and their sum is 12, which
means the last two positions that have to be worked some more must
sum to 8, since 12 + 8 would be 20, our initial number.

8 / 2 is 4. We fill the last two elements with 4.

![split int even parts v1](./assets/split-int-even-parts-1.png)

## TypeScript

### Unit Tests

```ts
--8<-- "docs/algorithms/split-int-even-parts/ts/splitInt.test.ts"
```

### v1

```ts
--8<-- "docs/algorithms/split-int-even-parts/ts/splitInt_v1.ts"
```

### v2

Another approach (thanks to Pedro Henrique Antunes de Oliveira):

- `n` is the number to be divided into `p` parts.
- `q` is the quotient and `r` the remainder of integer division.
- Such that this is true: `n * p + r = n`.

With 20 and 6, `20 / 6 = 3`, `3 * 6 = 18`, and `20 - 18 = 2`.
We fill the array with six 3's, but we still need to add 1 to two of those elements.

```text
[3, 3, 3, 3, 3, 3] -> 18

[3, 3, 3, 3, 3 + 1, 3 + 1]
                ^      ^

[3, 3, 3, 3, 4, 4] -> 12 + 8 = 20
```

```ts
--8<-- "docs/algorithms/split-int-even-parts/ts/splitInt_v2.ts"
```

## References

- [Split a number into n numbers (Math StackExchange)](https://math.stackexchange.com/questions/2975936/split-a-number-into-n-numbers).
- [How to divide an unknown integer into a given number of (almost) even integers (StackOverflow)](https://stackoverflow.com/questions/56107612/how-to-divide-an-unknown-integer-into-a-given-number-of-almost-even-integers/64245018).
- [Split number into equal parts or what's closest to that](https://stackoverflow.com/questions/36414923/split-number-into-equal-parts-or-whats-closest-to-that).
