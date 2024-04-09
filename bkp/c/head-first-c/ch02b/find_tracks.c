#include <stdio.h>
#include <string.h>

// Each string will have at most 79 chars.
#define MAX_STR_LEN 80

// We have 5 strings.
#define MAX_ARR_LEN 5

// An array of strings, which is an array of arrays.
char tracks[MAX_ARR_LEN][MAX_STR_LEN] = {
  "I left my heart in Harvard Med School",
  "Newark, Newark - a wonderful town",
  "Dancing with a Dork",
  "From here to maternity",
  "The girl from Iwo Jima",
};

/**
 * ‘q’ stands for “query”. It is a common (short) name for
 * query/search params.
 */
void find_track (char *q)
{
  short i;

  for (i = 0; i < MAX_ARR_LEN; ++i) {
    // printf ("%s, %s\n", q, *(tracks + i));
    if (strstr (*(tracks + i), q)) {
      printf ("Track %hi: “%s”\n", i, *(tracks + i));
    }
    else {
      printf ("%p\n", strstr (tracks[i], q));
    }
  }
}


int main ()
{
  char query[MAX_STR_LEN];
  printf ("Search for: ");
  fgets (query, MAX_STR_LEN, stdin);

  /*
   * If last char is not ‘\0’, make it so in order
   * for ‘strstr’ to work fine.
   */
  if (*(query + (strlen (query) - 1)) == '\n') {
    *(query + (strlen (query) - 1)) = '\0';
  }

  // Using subscript notation.
  // if (query[strlen (query) - 1] == '\n') {
  //   query[strlen (query) - 1] = '\0';
  // }

  find_track (query);

  return 0;
}

/* vim: set syn=off ft=text ai: */

