
/**
 * Checks whether the `c` is an alphanumeric character.
 *
 * @param {string} A 1-character string.
 * @return {boolean}
 */
function isAlphaNum(c) {
  return /[a-z0-9]/.test(c);
}

/**
 * Count the frequency of alphanumeric chars in the input string.
 *
 * This third version finally makes sure we ignore non-alphanumeric
 * characters through the use of the `/[a-z0-9]/`  regexp.
 *
 * @param {string} s
 * @return {object} An object whose keys are the char counted
 *   and the value is the number occurrences of that char.
 *
 * @example
 * charCount("racecar");
 * // → { r: 2, a: 2, c: 2, e: 1}
 *
 * @example
 * charCount("hey");
 * // → { h: 1, e: 1, y: 1 }
 */ 
function charCount(s) {
  const frequencies = {};

  for (let i = 0; i < s.length; ++i) {
    let c = s[i].toLowerCase();

    if (!isAlphaNum(c)) continue;

    if (frequencies[c] === undefined)
      frequencies[c] = 1;
    else frequencies[c] += 1;
  }

  return frequencies;
}

module.exports = { charCount };
