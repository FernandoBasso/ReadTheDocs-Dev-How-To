#include <stdio.h>

/**
 * (origin)
 *     ·
 *       ·
 *         ·
 *           ·
 *             ·
 *        (destination)
 *
 * Moving south-east means:
 *  - latitude decreases
 *  - longitude increases
 */
void go_south_east (short *lat, short *lon)
{
  *lat = *lat - 1;
  *lon = *lon + 1;
}

int main ()
{
  short latitude = 0;
  short longitude =   0;

  //
  // Passing memory locations (pointers). The updates are
  // performed on the original ‘latitude’ and ‘longitude’
  // variables (not on copies of them).
  //
  go_south_east (&latitude, &longitude);

  fprintf (stdout,
           "Avast! Now at: [%i, %i]\n",
           latitude, longitude);

  return 0;
}




/* vim: set syntax=off filetype=text autoindent: */

