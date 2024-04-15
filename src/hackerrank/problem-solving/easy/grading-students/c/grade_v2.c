#include <stdio.h>
#include <stdlib.h>
#include <math.h>

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

