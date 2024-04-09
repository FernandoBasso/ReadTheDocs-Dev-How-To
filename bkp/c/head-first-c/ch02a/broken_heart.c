#include <stdio.h>

void skip (char *msg)
{
  printf ("%s\n", (msg + 6));
  // →  call me
}

int main ()
{
  char *msg_from_amy = "Don't call me";
  skip (msg_from_amy);

  return 0;
}

