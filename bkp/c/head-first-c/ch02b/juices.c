#include <stdio.h>
#include <string.h>

void print_reverse (const char *s)
{
  size_t len = strlen (s);

  /* Last char of the string. */
  const char *t = s + len - 1;

  while (t >= s) {
    printf ("%c", *t);
    t -= 1;
  }

  printf ("\n");
}

int main ()
{
  char *juices[] = {
    "dragonfruit",
    "waterberry",
    "sharonfruit",
    "uglifruit",
    "rumberry",
    "kiwifruit",
    "mulberry",
    "strawberry",
    "blueberry",
    "blackberry",
    "starfruit",
  };

  char *a;

  puts (juices[6]);
  print_reverse (juices[7]);
  a = juices[2];
  juices[2] = juices[8];
  juices[8] = a;
  print_reverse (juices[(18 + 7) / 5]);

  puts (juices[2]);
  print_reverse (juices[9]);
  juices[1] = juices[3];
  puts (juices[10]);
  print_reverse (juices[1]);

  return 0;
}

/* vim: set syn=off ft=text ai: */

