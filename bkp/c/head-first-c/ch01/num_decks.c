#include <stdio.h>

/**
 * ‘int’ here indicates the _return type_ of the function.
 */
int main ()
{
  int decks;

  puts("Enter a number of decks:");
  scanf("%i", &decks);

  if (decks < 1)
  {
    puts ("That is not a valid number of decks.");
    return 1;
  }

  printf ("There are %i cards\n.", (decks * 52));

  return 0;
}

/*
 * From a *nix shell, ‘echo $?’ shows the exit status of
 * our program. On windows, ‘echo %ErrorLevel%’.
 */

