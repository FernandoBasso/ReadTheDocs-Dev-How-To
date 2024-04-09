#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * Removes first and last chars of the input string.
 *
 * ASSUME: Input string is never less than 3, including '\0'.
 */
const char* chop_sides (char* dst, const char* src) {

  const char *orig = src;

  // Skip the first char.
  ++src;

  // Copy until one char before the '\0'.
  while (*(src + 1) != 0 && (*dst++ = *src++))
    ; /* <1> */

  // Add the NUL terminator.
  *dst = '\0';

  // <2>
  return orig;
}


int main ()
{
  unsigned int i;

  char *strings[] = {"eloquent", "country", "person", "place", "ok"};

  char *chopped;

  for (i = 0; i < sizeof(strings) / sizeof(char*); ++i) {
    const size_t size = strlen(strings[i]) + 1;

    chopped = calloc(size, size * sizeof(char));

    chop_sides(chopped, strings[i]);

    printf("%10s: %s\n", strings[i], chopped);

    free(chopped);
  }

  return 0;
}

/*
 * This is the output:
 *
  eloquent: loquen
   country: ountr
    person: erso
     place: lac
        ok:
*/

/*

<1>: The null statement.

<2>: If we returned `dst`, we would return the pointer at the last position,
precisely the place where we added the NUL terminator. We would be returning an
empty thing, not the pointer from the beginning of the copied string.

We could just as well have written a void function.

*/


