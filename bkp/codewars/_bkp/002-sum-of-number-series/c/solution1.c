#include <stdio.h>

//
// Define MIN and MAX macros.
//
// https://stackoverflow.com/a/3437640/2855955
//
#define MIN(x, y) ((x < y) ? x : y)
#define MAX(x, y) ((x > y) ? x : y)

int get_sum (int a, int b)
{
  //
  // Find out min and max.
  //
  int min = MIN(a, b);
  int max = MAX(a, b);

  //
  // Using min and max, calculate the number of elements in the series.
  //
  int n = max - min + 1;

  //
  // Apply the formula.
  //
  // https://www.mathwords.com/a/arithmetic_series.htm
  //
  return n * (max + min) / 2;
}

int main ()
{
  fprintf(stdout, "%d\n", get_sum (3, 5));
  // ⇒ 12, because 3 + 4 + 5 = 12.

  fprintf(stdout, "%d\n", get_sum (-2, 4));
  // ⇒ 7, because -2 + -1 + 0 + 1 + 2 + 3 + 4 = 7.

  fprintf(stdout, "%d\n", get_sum (1, 0));
  // ⇒ 1, because 1 + 0 = 1.

  fprintf(stdout, "%d\n", get_sum (1, 2));
  // ⇒ 3, because 1 + 2 = 3.

  fprintf(stdout, "%d\n", get_sum (0, 1));
  // ⇒ 1, because 0 + 1 = 1.

  fprintf(stdout, "%d\n", get_sum (1, 1));
  // ⇒ 1, because both operands are the same number.

  fprintf(stdout, "%d\n", get_sum (-1, 0));
  // ⇒ -1, because -1 + 0 = -1.

  fprintf(stdout, "%d\n", get_sum (-1, 2));
  // ⇒ 1, because -1 + 2 = -1.

  return 0;
}

