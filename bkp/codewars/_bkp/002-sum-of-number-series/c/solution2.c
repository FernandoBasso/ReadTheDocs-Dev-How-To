#include <stdio.h>
#include <stdlib.h>

int get_sum (int a, int b)
{
  //
  // Find the number of elements in the sequence using absolute values, wich
  // allows us not to worry which is the smaller and the greater number.
  //
  int n = abs(a - b) + 1;

  //
  // Apply the formula.
  //
  // https://www.mathwords.com/a/arithmetic_series.htm
  //
  printf("n %d, a %d, b %d\n", n, a, b);
  return n * (a + b) / 2;
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
  // ⇒ 2, because -1 + 0 + 1 + 2 = 2.

  return 0;
}

