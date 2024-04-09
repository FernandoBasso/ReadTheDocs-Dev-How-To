#include <stdio.h>

int main ()
{
  short doses[] = {1, 3, 2, 1000};

  fprintf (stdout, "%hi\n", 3[doses]);
  // →  1000

  return 0;
}

//
// These all mean the same thing:
//
//   3[doses]
//   doses[3]
//   *(doses + 3)
//   *(3 + doses)
//

