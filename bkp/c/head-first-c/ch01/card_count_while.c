#include <stdio.h>
#include <stdlib.h>

int main ()
{
  char card_name[3];
  short count = 0;

  while (card_name[0] != 'X') {
    printf ("Enter card name: ");
    scanf ("%2s", card_name);
    int val = 0;

    switch (card_name[0]) {
      case 'K':
      case 'Q':
      case 'J':
        val = 10;
        break;
      case 'A':
        val = 11;
        break;
      case 'X':
        printf ("Quitting!\n");
        continue;
      default:
        val = atoi (card_name);
        if (1 > val || val > 10) {
          fprintf (stderr, "Invalid range... 💩\n\n");
          continue;
        }
    }

    if (2 < val && val < 7) {
      count += 1;
    } else if (val == 10) {
      count -= 1;
    }

    printf ("Current count: “%i”\n", count);
  }

  return 0;
}

