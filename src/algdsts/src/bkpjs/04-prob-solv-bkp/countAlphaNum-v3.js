////////////////////////////////////////////////////////////////////////
// My solution based on K&R C book which mentions numerical values for
// characters. Unlike in C, chars (strings of one char) do not
// automatically get an integer representation, so, we use
// `charCodeAt()` to help us out.
//

const CHAR_CODE_a = "a".charCodeAt(0); // 97
const CHAR_CODE_z = "z".charCodeAt(0); // 122
const CHAR_CODE_A = "A".charCodeAt(0); // 65
const CHAR_CODE_Z = "Z".charCodeAt(0); // 90
const CHAR_CODE_0 = "0".charCodeAt(0); // 48
const CHAR_CODE_9 = "9".charCodeAt(0); // 57

/**
 * Verifies if a character is a letter or digit.
 *
 * @param {string} chr A 1 character string.
 * @return {boolean}
 */
function isAlphaNum(chr) {
  const code = chr.charCodeAt(0);

  return (code >= CHAR_CODE_a && code <= CHAR_CODE_z) ||
    (code >= CHAR_CODE_A && code <= CHAR_CODE_Z) ||
    (code >= CHAR_CODE_0 && code <= CHAR_CODE_9);
}

/**
 * Count the number of alphanumeric characters in the input string.
 *
 * **TIME COMPLEXITY**: `O(n)`. We iterate over every element in `str`.
 *
 * **SPACE COMPLEXITY**: `O(1)`. There is only a specific subset of
 * chars we count (alphanumeric), so, the produced accumulator object
 * never gets larger than that. We can consider it constant space.
 *
 * @param {string}
 * @return {object}
 */
function countAlphaNum(str) {
  if (str.length === 0) return {};

  let freq = {};

  for (let chr of str) {
    if (isAlphaNum(chr)) {
      const k = chr.toLowerCase();
      freq[k] = ++freq[k] || 1;
    }
  }

  return freq;
}

export { countAlphaNum };
