#include <stdio.h>

int main ()
{
  // Set aside forty bytes of RAM.
  char name[40];
  short age;

  fprintf (stdout, "Name: ");
  // Update the data in the previously set aside memory
  // with user-provided string.
  fscanf (stdin, "%39s", name);

  fprintf (stdout, "Age: ");
  fscanf (stdin, "%hd", &age);

  printf ("Name is: “%s”\n", name);
  printf ("Age is: %hi\n", age);

  return 0;
}

//
// NOTE: Since ‘name’ is an array, it is a pointer. We don't
// pass it to ‘scanf’ using the address of operator ‘&’.
//
// ‘%s’ reads only up to the first space. To read a name and
// a surname, use something like ‘scanf ("%s %s", name, surname)’.
//

/* vim: set syn=off ft=text ai: */

