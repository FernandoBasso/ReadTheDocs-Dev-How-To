/*
 * EXAMPLE USAGE:
 *
 *   $ ./prog -d 'for dinner'
 *   To be delivered for dinner.
 *
 *   $ ./prog -t -d 'for dinner'
 *   Thick crust.
 *   To be delivered for dinner.
 *
 * NOTES
 * -----
 *
 * Compiling with ‘-std=c99’ prevents the POSIX macros from
 * being defined in ‘<features.h>’, which prevents ‘<unistd.h>’
 * from including ‘<getopt.h>’, therefore, compile without
 * ‘-std=c99’.
 *
 * Another option is not including ‘<unistd.h>’ and instead
 * include ‘<getopt.h>’. Then we can use ‘-std=c99’ with GCC.
 *
 * See this:
 *
 * https://stackoverflow.com/questions/22575940/getopt-not-included-implicit-declaration-of-function-getopt
 */

#include <stdio.h>
#include <getopt.h>

int main (int argc, char *argv[])
{
  char *delivery = "";
  int thick = 0;
  int count = 0;
  char ch;

  /* <1> */
  while ((ch = getopt (argc, argv, "d:t")) != EOF) {
    switch (ch) {
      case 'd':
        /* <2> */
        delivery = optarg;
        break;
      case 't':
        thick = 1;
        break;
      default:
        fprintf (stderr, "Unknown option “%s”.\n", optarg);
        return 1;
    }
  }

  /* <3> */
  argc -= optind;
  argv += optind;

  if (thick)
    printf ("Thick crust.\n");

  if (delivery[0])
    printf ("To be delivered %s.\n", delivery);

  for (count = 0; count < argc; ++count)
    printf ("%s\n", argv[count]);

  return 0;
}


/*
 * <1> ‘d:’ means ‘-d’ option must be followed by an argument. ‘t’
 * is just a “on/off” flag. No arguments required.
 *
 * <2> ‘optarg’ id the argument for ‘d’. It is magically
 * made available for us.
 *
 * <3> ‘optind’ is also magically made available. It store the
 * number of strings read from the command line...  ... so we can
 * use it to make sure we skip past the command line options.
 */

/* vim: set syn=off ft=text ai tw=68 cc=+1,+10: */

