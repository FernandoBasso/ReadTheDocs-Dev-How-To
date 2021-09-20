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
  const trimmed = value.trim();
  const num = parseInt(trimmed, 10);
  const nextNum = num + 1;
  return String.fromCharCode(nextNum);
}

export { nextCharFromNumStr }
