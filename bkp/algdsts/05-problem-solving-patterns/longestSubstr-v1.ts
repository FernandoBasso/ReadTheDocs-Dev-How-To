//
// Sliding Window - findLongestSubstr
//
// Write a function called findLongestSubstr, which accepts a string
// and returns the length of the longest substring with all distinct
// characters.
//
// findLongestSubstr('') // 0
// findLongestSubstr('rithmschool') // 7
// findLongestSubstr('thisisawesome') // 6
// findLongestSubstr('thecatinthehat') // 7
// findLongestSubstr('bbbbbb') // 1
// findLongestSubstr('longestsubstring') // 8
// findLongestSubstr('thisishowwedoit') // 6
//
// Time Complexity - O(n)
//

/**
 * Finds length of longest substring of distinct chars.
 *
 * **TIME COMPLEXITY**: `O(n)` because we loop over the input once.
 *
 * **SPACE COMPLEXITY**: `O(n)` since we store seen chars in an object
 * which could potentially be as big as the input string.
 *
 * @param {string} str
 * @return {number}
 */
function findLongestSubstr(str) {
  let acc = {};
  let longestSoFar = 0;
  let tmpLongest = 0;
  let l = 0;
  let r = 0;

  while (l < str.length && r < str.length) {
    if (acc[str[r]]) {
      ++l;
      r = l + 1;
      acc = { [str[l]]: 1 };
      tmpLongest = 1;
      continue;
    }

    acc[str[r]] = 1;

    ++tmpLongest;
    ++r;

    longestSoFar = Math.max(tmpLongest, longestSoFar);
  }

  return longestSoFar;
}

export { findLongestSubstr };
