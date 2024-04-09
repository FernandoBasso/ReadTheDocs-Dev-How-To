#include <stdio.h>

int main ()
{
  char masked[] = "Alive";
  char *jimy = masked;

  printf ("Masked: %s, Jimy: %s\n", masked, jimy);

  masked[0] = 'D';
  masked[1] = 'E';
  masked[2] = 'A';
  masked[3] = 'D';
  masked[4] = '!';

  printf ("Masked: %s, Jimy: %s\n", masked, jimy);
  // Both are DEAD because both variables point to the
  // same memory location.

  return 0;
}

/* vim: set syn=off ft=text ai: */

