---
title: Plus Minus | Problem Solving (easy) | HackerRank
description: Notes and explanations on the Plus Minus HackerRank challenge.
---

# Plus Minus

- [Plus Minus on HackerRank](https://www.hackerrank.com/challenges/plus-minus/)


The problem involves counting the frequencies of negative, zero, and positive numbers and the ratios of each of these three “categories” in the input.

In the example on the page:

```text
[1, 1, 0, -1, -1]
```

We have a ratio of $\frac{2}{5}$ positive numbers, $\frac{1}{5}$ zeroes, and $\frac{2}{5}$ negatives.

The sum of those ratios/fractions/percentages end up as 1 (100%).

One could break the problem into:

- Counting the frequencies.
- Computing the ratios.

To compute the ratios, simply divide the frequency of the category (negative, zero, positive) by the length of the input array.
If there are, say, 3 negatives, 1 zero, and 2 positives, the ratios would be (the array would contain 6 elements, thus we would divide by 6):

- $\frac{3}{6}$.
- $\frac{1}{6}$.
- $\frac{2}{6}$.

$$
\frac{3}{6} + \frac{1}{6} + \frac{2}{6} = \frac{6}{6} = 1
$$
