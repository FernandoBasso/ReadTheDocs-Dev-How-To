/*

https://www.geeksforgeeks.org/core-dump-segmentation-fault-c-cpp/

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * Get the length of a string.
 */
short length (char *s)
{
  short c = 0;
  while (*s++) ++c;
  return c;
}

/**
 * Copies src to dst.
 *
 * Very unsafe. Just for my simple, dumb exercise.
 *
 * ASSUME: dst has enough memory allocated.
 */
void copy (char *dst, char *src)
{
  while ((*dst++ = *src++) != '\0')
    ;

  dst = '\0';
}


/**
 * Removes first and last chars of the input string.
 *
 * ASSUME: Input string is never less than 3 chars long.
 */
char *chop_sides (char s[], int len) {
  ++s;
  *(s + len - 3) = '\0';
  return s;
}


int main ()
{
  unsigned int i;

  // <1> We have string literals here. Can't do strings[1][0] = 'x', for example.
  char *strings[] = { "eloquent", "country", "person", "place" };

  char *string;
  char *chopped;

  for (i = 0; i < sizeof(strings) / sizeof(char*); ++i) {
    //
    // + 1 so we have space for the NUL terminator.
    //
    const size_t size = length(strings[i]) + 1;

    string = calloc(size, size * sizeof(char));
    chopped = calloc(size, size * sizeof(char));


    copy(string, strings[i]);

    chopped = chop_sides(string, size);

    printf("%10s: %s\n", string, chopped);

    // free(chopped);
  }

  return 0;
}

/*


<1>: `char *a = "Hello, world!\n"; a[1] = 'o';` This is not allowed.

char *strings[] = {"a", "bcd", "ef"};

Is it possible to compute the size of each string in the array?

Tried inside a loop, sizeof(strings[i]) * sizeof(char), but it looks like it
always gets the size of the first one, or something like that.

sizeof(strings[1]) is a pointer and will always return the same number

technically it's an array of pointers to char, so no.

i believe gcc and clang can do strlen at compile time, no?

you can do

#define a "a"
#define b "bcd"
char *strings[] = {a, b};

then sizeof(a); will work, but that's not really what you want

also if the elements are not expected to change then define it
const char * const strings[]=...

and use strlen() at runtime to get each string length, unless you see a good
reason not to.

https://www.geeksforgeeks.org/core-dump-segmentation-fault-c-cpp/
*/
