---
title: Grading Students :: HackerRank Easy Challenge
description: Solutions with notes and explanations for the Grading Students HackerRank Challenge in a few different languages
---

# Grading Students

- [Grading Students :: HackerRank Easy Challenge](https://www.hackerrank.com/challenges/grading)

## Multiples of an integer

Part of the solution to this challenge involves calculating the next multiple of a given integer.
Examples:

- Given 3, what is the next multiple of 5?
  5.
- Given 7, what is the next multiple of 5?
  10.
- Given 89, what is the next multiple of 5?
  90.
- Given 91, what is the next multiple of 5?
  95.

One way to find such next multiple would be to start from that number, and check if it the remainder of the division by the multiple we want is 0.
If not, keep incrementing that number until we eventually find the multiple:

```c
#include <stdio.h>

/**
 * Finds the next multiple of `n` given the multiplier `m`.
 *
 * - T.C: O(n).
 */
short next_mult_of(short m, short n) {
  while ((n % m) != 0)
    ++n;

  return n;
}

int main(int argc, char* argv[]) {
  short xs[6] = { 3, 4, 5, 9, 89, 98 };

  for (short i = 0; i < 6; ++i)
    printf("%hd\n", next_mult_of(5, *(xs + i)));

  return 0;
}
```

Example output:

```text
$ gcc -std=c99 -Wall -pedantic -o ./mult_of ./mult_of_v1.c \
    && ./mult_of
5
5
5
10
90
100
```

It works but it has time complexity $O(n)$ because we are looping as long as $m \pmod{5} = 0$.

A constant time approach is possible by a formula like this:

$$
\lceil{\frac{number}{multiplier}}\rceil \times multiplier
$$

That is, `ceil(number * multiplier) / multiplier`. In C:

```c
#include <stdio.h>
#include <math.h>

/**
 * Finds the next multiple of `n` given the multiplier `m`.
 *
 * - T.C: O(1)
 */
short next_mult_of(double m, short n) {
  return ceil(n / m) * m;
}

int main(int argc, char* argv[]) {
  short xs[6] = { 3, 4, 5, 9, 89, 98 };

  for (short i = 0; i < 6; ++i)
    printf("%hd\n", next_mult_of(5, *(xs + i)));

  return 0;
}
```

Compile and run with (note the `-lm` flag to link the math lib):

```text
$ gcc -std=c99 -Wall -pedantic -lm -o ./mult_of ./mult_of_v2.c \
    && ./mult_of
5
5
5
10
90
100
```

To be continued.
