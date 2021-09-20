/**
 * Produces the next char based on the numeric input string.
 *
 * ASSUME: The input is a valid numeric value.
 *
 * ASSUME: Client code is passing in a numeric value which
 * produces the character they want.
 *
 * @param {string} value The numeric string.
 * @return {string} The computed character.
 *
 * @sig 1String -> 1String
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
  return String.fromCharCode(parseInt(value.trim(), 10) + 1);
}

export { nextCharFromNumStr }
