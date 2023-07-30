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

A constant time approach is possible by a formula like this (shared by my friend Bijay Tamang):

- Let $m$ be the multiplier.
- Let $n$ be the number whose next multiple of $m$ is to be found.
- Then apply the formula $\lceil{\frac{n}{m}}\rceil \times {m}$.

That is, `ceil(number * multiplier) / multiplier`. In C:

```c
#include <stdio.h>
#include <math.h>

/**
 * Finds the next multiple of `n` given the multiplier `m`.
 *
 * - T.C: O(1)
 */
short next_mult_of(short m, short n) {
  return ceil(n / (double) m) * m;
}

int main(int argc, char* argv[]) {
  short xs[6] = { 3, 4, 5, 9, 89, 98 };

  for (short i = 0; i < 6; ++i)
    printf("%hd\n", next_mult_of(5, *(xs + i)));

  return 0;
}
```

We are casting `m` to `double` because long division with `/` works on fractional values.

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

Then we just loop and apply the logic as per the instructions.

## C

### Solution 1 :: C

In this solution, static memory allocation is used for the `result` array,
which sets aside storage for 100 ints.

```c
#include <stdio.h>
#include <math.h>

#define GRADES_LEN 8
#define MAX_GRADES 100

/**
 * Finds the next multiple of `n` given the multiplier `m`.
 *
 * - T.C: O(1).
 * - S.C: O(1).
 */
int next_mult_of(int m, int n) {
  return ceil(n / (double) m) * m;
}

/**
 * Apply the grading logic to the grades.
 *
 * - T.C: O(n).
 * - S.C: O(n).
 */
int* grade(int grades_len, int* grades, int* res_len) {
  int i, grade;
  static int res[MAX_GRADES];

  printf("len %d\n", grades_len);

  for (
    i = 0, grade = *(grades + i);
    i < grades_len;
    ++i, grade = *(grades + i)
  ) {
    if (grade < 38) {
      res[i] = grade;
      continue;
    }

    int new_grade = next_mult_of(5, grade);

    res[i] = (new_grade - grade < 3) ? new_grade : grade;
  }

  *res_len = i;

  return res;
}

int main(int argc, char* argv[]) {
  int i;
  int xs[GRADES_LEN] = { 3, 37, 38, 39, 40, 41, 89, 98 };
  int* result;
  int result_count;

  result = grade(GRADES_LEN, xs, &result_count);

  for (i = 0; i < result_count; ++i)
    printf("%d\n", *(result + i));

  return 0;
}
```

The space complexity is $O(n)$ because a new array is created to store the results of the grading.

### Solution 2 :: C

If resulting array does is dynamically memory allocated, then the third parameter `result_count` is unnecessary, as the resulting array is not hardcoded to length 100 any longer, but instead is allocated to the same length as the input grades array.

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/**
 * Apply the grading logic to the grades.
 *
 * - T.C: O(n).
 * - S.C: O(n).
 */
int* grade(int len, int* grades) {
  int i, grade;
  int* res = malloc(len * sizeof(int));

  for (i = 0; i < len; ++i) {
    grade = *(grades + i);

    if (grade < 38) {
      res[i] = grade;
      continue;
    }

    int new_grade = next_mult_of(5, grade);

    res[i] = (new_grade - grade < 3) ? new_grade : grade;
  }

  return res;
}

int main(int argc, char* argv[]) {
  int i;
  int grades[] = { 3, 37, 38, 39, 40, 41, 75, 83, 84, 98 };
  int* result;
  int len = sizeof grades / sizeof(int);

  result = grade(len, grades);

  for (i = 0; i < len; ++i)
    printf("%d\n", *(result + i));

  return 0;
}
```

Again, space complexity is $O(n)$ because the computed grades are stored in a resulting array.

### Solution 3 :: C

Let's switch from `int` to `short` (just to use a different type once, shall we‽

First, let's reconsider our “multiple of 5” thing, just so we find other solutions to the problem.

We can do it with another math approach.

If the grade is 47, then:

```
47 + 5 = 52
52 % 5 = 2
52 - 2 = 50
          \
           \
            v
  50 is the next multiple of 5 starting from 47.
```

If grade is 48, then:

```
48 + 5 = 53
53 % 5 = 3
53 - 3 = 50
          \
           \
            v
  50 is the next multiple of 5 starting from 48.
```

If grade is 49, then:

```
49 + 5 = 54
54 % 5 = 4
54 - 4 = 50
          \
           \
            v
  50 is the next multiple of 5 starting from 49.
```

If grade is 50, then it is already a multiple of 5 and no rounding should occur.

Considering the one's place, any value of 1, 2, 3, 6, or 7 means the grade should not round.
Values of 4, 8 and 9 means the grade should be rounded.

Example in the range of 41 to 49:

- 41, 42, 43: do not round.
- 46, 47: do not round.
- 44: round up to 50.
- 48, 49: round up to 50.

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/**
 * Finds the next multiple of `n` given the multiplier `m`.
 *
 * - T.C: O(1).
 * - S.C: O(1).
 */
short next_mult_of(short m, short n) {
  return n + m - n % m;
}

/**
 * Apply the grading logic to the grades.
 *
 * - T.C: O(n).
 * - S.C: O(1).
 */
void grade(short len, short* grades) {
  short i, grade;

  for (i = 0; i < len; ++i) {
    grade = *(grades + i);

    if (grade < 38) continue;
    if (grade % 5 < 3) continue;

    *(grades + i) = next_mult_of(5, grade);
  }
}

int main() {
  short i;
  short grades[] = { 3, 37, 38, 39, 40, 41, 75, 83, 84, 98 };
  short len = sizeof grades / sizeof(short);

  grade(len, grades);

  for (i = 0; i < len; ++i)
    printf("%hd\n", *(grades + i));

  return 0;
}
```

The function `grade()` now has space complexity of $O(n)$ as we are modifying the input array in place (not necessarily a good or bad practice, as pros and cons always exist for almost any approaches taken and it all depends on the context).

The two `if` conditions could be made into a single one with an or `||`:
Also, the function `next_mult_of()` was used again, just inlining the logic would be fine too.

```diff
 void grade(short len, short* grades) {
   short i, grade;
 
   for (i = 0; i < len; ++i) {
     grade = *(grades + i);
 
-    if (grade < 38) continue;
-    if (grade % 5 < 3) continue;
+    if (grade < 38 || grade % 5 < 3) continue;

-   *(grades + i) = next_mult_of(5, grade);
+   *(grades + i) = grade + 5 - grade % 5;
   }
 }
```

But maybe using the utility function helps make things more self-documenting.

