#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * COMPILE:
 *
 * $ gcc std=c99 -Wall -pedantic -o categorize1 categorize1.c
 *
 * USAGE:
 *
 *  $ ./categorize
 *
 * NOTE: we do not pass any input to the program on the command
 * line. The program itself opens a file to read, and writes
 * to files. No command line redirection is used here. Check
 * ‘geo2json.c’ to compare.
 *
 * Then check the resulting files:
 *
 *  $ sed '' ufos.csv
 *  $ sed '' disappearances.csv
 *  $ sed '' others.csv
 */
int main ()
{
  char line[80];
  FILE *in = fopen ("spooky.csv", "r");
  FILE *file1 = fopen ("ufos.csv", "w");
  FILE *file2 = fopen ("disappearances.csv", "w");
  FILE *file3 = fopen ("others.csv", "w");

  while (fscanf (in, "%79[^\n]\n", line) == 1) {
    if (strstr (line, "UFO"))
      fprintf (file1, "%s\n", line);
    else if (strstr (line, "Disappearance"))
      fprintf (file2, "%s\n", line);
    else
      fprintf (file3, "%s\n", line);
  }

  /* REMEMBER: Always close the streams. */
  fclose (in);
  fclose (file1);
  fclose (file2);
  fclose (file3);

  return 0;
}


/* vim: set syn=off ft=text ai tw=68 cc=+1,+10: */

