//
// Examples of anagrams:
//
//            tar = rat
//            arc = car
//          elbow = below
//          state = taste
//          cider = cried
//          dusty = study
//          night = thing
//           inch = chin
//
//      dormitory = dirty room
//  school master = the classroom
//   conversation = voices rant on
//         listen = silent
//     astronomer = moon starer
//       the eyes = they see
//    a gentleman = elegant man
//        funeral = real fun
//

//
// To solve it, we must see if the second string has the same frequency
// of letters as in the first string. “iceman” is anagram to “cinema”.
// Each letter in iceman has a corresponding frequency in “cinama” (and
// vice-versa”.
//
// If a string contains spaces, ignore them. E.g.
//
//   “a gentleman” = “elegant man”
//
// When checking for the “anagramness” of the strings, ignore the spaces
// and any other punctuation character.
//
// NOTE: Do not confuse anagrams with palindromes. Palindromes are words
// that are the same if spelled backwards, like “racecar”. If you
// reverse it, it is still “racecar”.
//

/**
 * Checks whether the two input strings are anagrams to one another.
 *
 * ASSUME: Input are simple anagrams, not containing spaces or other non
 * lowercase letters.
 *
 * @param {string} s1
 * @param {string} s2
 * @return {boolean}
 *
 * **TIME COMPLEXITY**: `O(n)`. We loop a couple times for the length of
 * the input string. Not a nested loop.
 *
 * **SPACE COMPLEXITY**: `O(n)`. We create a frequency object of at most
 * the size of the input. This object may be as large as the input.
 *
 * NOTE: This is my solution. It involves looping and counting the
 * frequencies of characters in `s1`, and then using a second loop to
 * iterate over each key of `s1`, decrementing each occurrence of the
 * current char in the loop in the frequency object. Then, a second loop
 * checks whether any key has got a value count different than zero,
 * which means the frequencies don't match and the strings are not
 * anagrams to each other.
 */
function isAnagram(s1, s2) {
  if (s1.length !== s2.length) return false;

  if (s1.length === 0 && s2.length === 0) return true;

  const freqsS1 = {};

  for (let c of s1) {
    freqsS1[c] = (freqsS1[c] || 0) + 1;
  }

  for (let c of s2) {
    if (!freqsS1[c]) return false; // <1>

    freqsS1[c] -= 1;
  }

  return true;
}

export { isAnagram };

//
// <1> In case that char ‘c’ does not exist in ‘freqsS1’, it means it
// exists in ‘s2’ but not in ‘s1’, so the inputs are not anagrams.
// However, if it is 0 (which is also falsy), it is not an anagram
// either because it means we found it again (during this iteration of
// the loop) on ‘s2’ which means we would decrement it even more, making
// it ‘-1’. It means the current char doesn't appear with the same
// frequency on both strings.
//
