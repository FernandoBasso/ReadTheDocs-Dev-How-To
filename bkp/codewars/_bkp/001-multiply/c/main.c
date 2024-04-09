/*
 * Compile:
 *   gcc -std=c99 -Wall -Werror -pedantic main.c -o main
 *
 * Run:
 *   ./main
 */

#include <stdio.h>

int multiply (int x, int y)
{
  return x * y;
}

int main()
{
  fprintf(stdout, "%d\n", multiply(3, 6));
  // → 18

  fprintf(stdout, "%d\n", multiply(4, 11));
  // → 44
}

