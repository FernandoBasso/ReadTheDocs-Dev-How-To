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
  let longest = 0;
  let seen = {};
  let start = 0;

  for (let i = 0; i < str.length; i++) {
    let char = str[i];

    if (seen[char]) {
      start = Math.max(start, seen[char]);
    }

    // index - beginning of substring + 1 (to include current in count)
    longest = Math.max(longest, i - start + 1);

    // store the index of the next char so as to not double count
    seen[char] = i + 1;
  }

  return longest;
}

//
// Again, the solution from the instructor is more elegant and concise
// than mine 🥲.
//

export { findLongestSubstr };
