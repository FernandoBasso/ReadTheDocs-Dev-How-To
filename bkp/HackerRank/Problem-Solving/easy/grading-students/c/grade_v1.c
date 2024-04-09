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
short next_mult_of(short m, short n) {
  return ceil(n / (double) m) * m;
}

/**
 * Apply the grading logic to the grades.
 *
 * - T.C: O(n).
 * - S.C: O(n).
 */
short* grade(short grades_len, short* grades, short* res_len) {
  short i, grade;
  static short res[MAX_GRADES];

  for (
    i = 0, grade = *(grades + i);
    i < grades_len;
    ++i, grade = *(grades + i)
  ) {
    if (grade < 38) {
      res[i] = grade;
      continue;
    }

    short new_grade = next_mult_of(5, grade);

    res[i] = (new_grade - grade < 3) ? new_grade : grade;
  }

  *res_len = i;

  return res;
}

int main(int argc, char* argv[]) {
  short i;
  // short xs[GRADES_LEN] = { 3, 37, 38, 39, 40, 41, 89, 98 };
  short xs[MAX_GRADES] = { 4, 73, 67, 38, 33 };
  short* result;
  short result_count;

  result = grade(5, xs, &result_count);

  for (i = 0; i < result_count; ++i) {
    printf("%hd\n", *(result + i));
  }

  return 0;
}

