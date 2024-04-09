#include <stdio.h>

/**
 * A simple program to print which is the greater number.
 */

short larger (short x, short y)
{
  return x > y ? x : y;
}

int main ()
{
  int greatest = larger(5, 9);
  fprintf(stdout, "%i is the greatest!\n", greatest);

  return 0;
}

