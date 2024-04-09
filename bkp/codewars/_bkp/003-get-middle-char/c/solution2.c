//
// Strings passed as parameters to function become pointers.
// `sizeof(param)` will return the size of the type of the
// first element, not the size of the string.
//
// NOTE: There is no way for a function to know the size of
// a string parameter unless iterating over it or using some
// helper utility, like `strlen`.
//
// NOTE: We use the `3` in some places because C strings include
// the NUL terminator, which also counts for `sizeof`.
//

#include <stdio.h>
#include <math.h>

char* get_middle (char *s, short len)
{
  // If size is <= 3, no further operations are required, the string
  // is already two chars long plus the NUL terminator.
  if (len <= 3) return s;

  // Calculate index of the middle char.
  short idx = (short) floor((len - 1) / 2);

  // Advance the pointer to that index.
  s += idx;

  // If length is odd, we return precisely the one char in the middle.
  // -1 because we want to ignore the NUL terminator for the sake of
  // finding the middle char.
  if ((len - 1) % 2 != 0) {
    // Terminate the string after the middle char.
    *(s + 1) = '\0';
  }
  else {
    // Terminate the string after the second char from `idx`.
    // Note that at this point `s` is already advanced to a
    // new memory location.
    *(s + 2) = '\0';
  }

  return s;
}


int main()
{

  char s_c[] = {'C', '\0'};
  char s_os[] = "OS";
  char s_linux[] = "Linux";
  char s_typescript[] = "TypeScript";

  fprintf (stdout, "%s\n", get_middle(s_c, sizeof(s_c) / sizeof(char)));
  // ⇒ C

  fprintf (stdout, "%s\n", get_middle(s_os, sizeof(s_os) / sizeof(char)));
  // ⇒ OS

  fprintf (stdout, "%s\n", get_middle(s_linux, sizeof(s_linux) / sizeof(char)));
  // ⇒ n

  fprintf (stdout, "%s\n", get_middle(s_typescript, sizeof(s_typescript) / sizeof(char)));
  // ⇒ Sc

  return 0;
}
