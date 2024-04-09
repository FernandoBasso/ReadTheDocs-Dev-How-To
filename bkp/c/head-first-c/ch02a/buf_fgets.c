#include <stdio.h>

int main ()
{
  char buf[5];
  printf ("sizeof (buf): %lx\n", sizeof (buf));

  printf ("Input: ");
  fgets (buf, sizeof (buf), stdin);
  printf ("Val: %s\n", buf);

  return 0;
}

/*
 * ‘fgets’ ignore any further input beyond the limit imposed by the
 * limit/size param. But it can read onto only one pointer at a time.
 *
 *   man 3 fgets
 */

