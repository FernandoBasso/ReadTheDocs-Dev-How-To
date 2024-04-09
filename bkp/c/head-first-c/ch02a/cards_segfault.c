#include <stdio.h>

int main ()
{
  //
  // Segmentation fault when attempting to update the
  // string. Use ‘cards[]’ instead in this case.
  //
  char cards = "JQK";
  char a_card = *cards;
  //    J

  cards[2] = cards[1];
  //  Q

  cards[1] = cards[0];
  //  J

  cards[0] = cards[2];
  //  Q

  cards[2] = cards[1];
  //  J

  cards[1] = a_card;
  //  J

  puts(cards);

  return 0;
}

//
// And use ‘const char *cards’ to have the compiler abort with
// an error so you can catch the mistake.
//
