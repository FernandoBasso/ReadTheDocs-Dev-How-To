//
// Multiple Pointers - isSubSeq
//
// Write a function called isSubSeq which takes in two strings and
// checks whether the characters in the first string form a subsequence
// of the characters in the second string. In other words, the function
// should check whether the characters in the first string appear
// somewhere in the second string, without their order changing.
//
// Examples:
//
// isSubSeq('hello', 'hello world'); // true
// isSubSeq('sing', 'sting'); // true;
// isSubSeq('abc', 'abracadabra'); // true;
// isSubSeq('abc', 'acb'); // false (order matters)
//
// Your solution MUST have AT LEAST the following complexities:
// • Time Complexity: O(n + m)
// • Space Complexity: O(1)
//

/**
 * Checks whether `sub` is a subsequence of `str`.
 *
 * **TIME COMPLEXITY**: `O(n + m)`,
 *
 * ** SPACE COMPLEXITY**: `O(1)`.
 *
 * @param {string} sub The subsequence to try to find in `str`.
 * @param {string} str The string where the subsequence is to be
 *   searched in.
 * @param {boolean}
 */
function isSubSeq(sub, str) {
  if (sub.length > str.length) return false;

  let l = 0, r = 0;

  while (l <= sub.length && r <= str.length) {
    if (l === sub.length) return true; // <1>

    if (sub[l] === str[r]) { // <2>
      ++l;
      ++r;
    } else { // <3>
      ++r;
    }
  }

  return false;
}

export { isSubSeq };

//
// <1> If we get to the end of `sub` it means we have successfully found
// a subsequence.
//
// <2> If we find a matching pair of characters, increment both
// “pointers” so we check the next pair of characters.
//
// <3> If a match is not found during this iteration, it means we have
// yet to find a char in `str` which matches the current char in `seq`.
// Therefore, we increment only `r` so we keep searching further ahead
// in `str`.
//
// TIME COMPLEXITY: O(n + m). We use a single while, but it updates `l`
// and `r` in a way that may cause one to stop and the other keep going,
// and vice versa.
//
// SPACE COMPLEXITY: O(1). We do not require making copies of the
// string, or arrays of matched values. Only two extra primitive
// variables are used inside the function.
//
