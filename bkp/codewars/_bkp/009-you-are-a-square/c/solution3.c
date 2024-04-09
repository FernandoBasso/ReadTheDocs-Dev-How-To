//
// Compile with:
//
//   gcc -std=c99 -Wall -Werror -pedantic solution1.c -o solution1 -lm
//

#include <stdio.h>
#include <stdbool.h>
#include <math.h>

bool is_square (int n)
{
  //
  // Less than zero cannot every be a square root.
  //
  if (n < 0) return false;

  double sqrt_of_n = sqrt(n);

  //
  // Uses this multiplication approach instead of division and
  // checking for the remainder.
  //
  return n == sqrt_of_n * sqrt_of_n;
}

int main ()
{
  fprintf(stdout, "%d\n", is_square(-1));
  // ⇒ false, -1 is not a perfect square
  // `sqrt': Numerical argument is out of domain - "sqrt" (Math::DomainError)

  fprintf(stdout, "%d\n", is_square(0));
  // ⇒ 0/true, 0 is a perfect square (0 * 0)

  fprintf(stdout, "%d\n", is_square(3));
  // ⇒ 1/false, 3 is not a perfect square

  fprintf(stdout, "%d\n", is_square(4));
  // ⇒ 0/true, 4 is a perfect square (2 * 2)

  fprintf(stdout, "%d\n", is_square(25));
  // ⇒ 0/true, 25 is a perfect square (5 * 5)

  fprintf(stdout, "%d\n", is_square(26));
  // ⇒ 1/false, 26 is not a perfect square

  return 0;
}

//
// REMEMBER: In C (and most shells), zero means success/true, and any other
// non-zero number means some sort of failure, falsy status or exit code.
//
// NOTE: `%` can only oprate on integers. That is why we use `fmod` from 'math.h'.
//
