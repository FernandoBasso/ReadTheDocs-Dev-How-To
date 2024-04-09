#include <stdio.h>
#include <string.h>

/**
 * Print s in reverse order.
 */
void print_rev (const char *s)
{
  size_t len = strlen (s);

  /* Last char of the string. */
  const char *t = s + len - 1;

  while (t >= s) {
    printf ("%c", *t);

    t = t - 1;
  }

  printf ("\n");
}

int main ()
{
  char s[] = "Master Yoda!";

  print_rev (s);

  return 0;
}

/* vim: set syn=off ft=text ai: */

