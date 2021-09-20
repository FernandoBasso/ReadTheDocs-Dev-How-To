/**
 * Produces the next char based on the numeric input string.
 *
 * ASSUME: The input is a valid numeric value.
 *
 * ASSUME: Client code is passing in a numeric value which
 * produces the character they want.
 *
 * @sig 1String -> 1String
 *
 * @param {string} value The numeric string.
 * @return {string} The computed character.
 *
 * @example
 * nextCharFromNumStr('64');
 * // → 'A'
 *
 * @example
 * nextCharFromNumStr(' 121  ');
 * // → 'z'
 */
function nextCharFromNumStr(value) {
  return [value].map(s => s.trim())
                .map(s => parseInt(s, 10))
                .map(i => i + 1)
                .map(i => String.fromCharCode(i))[0]; /* (1) */
}

export { nextCharFromNumStr }
