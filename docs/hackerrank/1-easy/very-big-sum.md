---
title: A Very Big Sum | HackerRank Challenges
description: Notes and solutions for Big Sum Hacker Rank Challenge.
---

# A Very Big Sum

- [A Very Big Sum challenge on HackerRank](https://www.hackerrank.com/challenges/a-very-big-sum/problem).

## TypeScript

The TypeScript version of this challenge sounds very difficult because they mention things like `LONG_INTEGER` and `LONG_INTEGER_ARRAY`, which don't matter for languages like ECMAScript (JavaScript, TypeScript) or other languages that don't bother too much with different numeric types.
It was probably ported from C or C++, or some other language where you can't store a `long int` into an `int` (or other similar situations) without losing precision.

### Test Cases

```{literalinclude} /../src/hackerrank/typescript/1-easy/big-sum/bigSum.test.ts
```

### v1 Procedural

Using a for loop with a simple, procedural style.

```{literalinclude} /../src/hackerrank/typescript/1-easy/big-sum/bigSum-v1.ts
```
