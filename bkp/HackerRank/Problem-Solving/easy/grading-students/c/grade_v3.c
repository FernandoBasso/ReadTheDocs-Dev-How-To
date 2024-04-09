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

    if (grade < 38 || grade % 5 < 3) continue;

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

