////////////////////////////////////////////////////////////////////////
// This solution based on K&R C book which mentions numerical values
// for characters. Unlike in C, chars (strings of one char) do not
// automatically get an integer representation, so, we use
// `charCodeAt()` to get the integer value of the character.
//

const CHAR_CODE_a = "a".charCodeAt(0); // 97
const CHAR_CODE_z = "z".charCodeAt(0); // 122
const CHAR_CODE_A = "A".charCodeAt(0); // 65
const CHAR_CODE_Z = "Z".charCodeAt(0); // 90
const CHAR_CODE_0 = "0".charCodeAt(0); // 48
const CHAR_CODE_9 = "9".charCodeAt(0); // 57

/**
 * Checks whether a character is a letter or digit.
 *
 * @param chr A 1 character string.
 * @return {boolean} `true` if `chr` is alphanumeric; `false` otherwise.
 */
function isAlphaNum(chr: string): boolean {
  const code = chr.charCodeAt(0);

  return (code >= CHAR_CODE_a && code <= CHAR_CODE_z) ||
    (code >= CHAR_CODE_A && code <= CHAR_CODE_Z) ||
    (code >= CHAR_CODE_0 && code <= CHAR_CODE_9);
}

/**
 * Count the frequencies of alphanumeric characters in the input string.
 *
 * **TIME COMPLEXITY**: `O(n)`. We iterate over every element in `str`.
 *
 * **SPACE COMPLEXITY**: `O(1)`. There is only a specific subset of
 * chars we count (alphanumeric), so, the produced accumulator object
 * never gets larger than that. We can consider it constant space.
 *
 * @param str The string to count the frequencies from.
 * @returns A map-like object with the char frequencies.
 */
function countAlphaNum(str: string): Record<string, number> {
  if (str.length === 0) return {};

  const freq: Record<string,number> = {};

  for (const c of str) {
    if (isAlphaNum(c)) {
      const chr = c.toLowerCase();
      freq[chr] = ++freq[chr] || 1;
    }
  }

  return freq;
}

export { countAlphaNum };
