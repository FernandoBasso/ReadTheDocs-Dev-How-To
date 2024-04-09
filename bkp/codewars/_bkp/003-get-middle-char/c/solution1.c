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

char* get_middle (char *s, short len)
{
  //
  // The base case.
  //
  if (len <= 3) {
    return s;
  }


  //
  // Remove first and last chars.
  //

  //
  // 1. Increment the pointer so we get past the first char.
  //
  ++s;

  //
  // 2. Replace the last char with '\0'. -3 because \0 on the
  // original string counts too _and_ strings are zero-indexed.
  //
  s[len - 3] = '\0';

  //
  // Recurse. Pass the new “version” of the string, and decrease 2
  // from the length param since we now “removed” two chars.
  //
  return get_middle(s, len - 2);
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
