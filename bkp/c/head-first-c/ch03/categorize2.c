#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * COMPILE:
 *
 * $ gcc std=c99 -Wall -pedantic -o categorize2 categorize2.c
 *
 * USAGE:
 *
 *  $ ./main str1 file1 str2 file2 file3
 *
 * EXAMPLE:
 *
 *  $ ./main UFO ufos.csv Elvis elvises.csv the_rest.csv
 *
 * Then check the resulting files:
 *
 *  $ cat ufos.csv
 *  $ cat elvises.csv
 *  $ cat the_rest.csv
 *
 */
int main (int argc, char *argv[])
{
  /*
   * Wee need to provide 5 args, but remember that argv[0]
   * always contains the name of the program itself.
   */
  if (argc != 6) {
    fprintf (stderr,
             "ERROR: You need to provide 5 arguments.\n");
    return 1;
  }

  char line[80];
  FILE *in;

  /*
   * If we can't open the file, tell the user and
   * exit with a non-zero status to indicate failure.
   */
  if (!(in = fopen ("spooky.csv", "r"))) {
    fprintf (stderr, "Can't open “spooky.csv”\n");
    return 1;
  }


  /*
   * Not checking if we can create wite streams,
   * but, let's assume we can.
   */
  FILE *file1 = fopen (argv[2], "w");
  FILE *file2 = fopen (argv[4], "w");
  FILE *file3 = fopen (argv[5], "w");

  while (fscanf (in, "%79[^\n]\n", line) == 1) {
    if (strstr (line, argv[1]))
      fprintf (file1, "%s\n", line);
    else if (strstr (line, argv[3]))
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


