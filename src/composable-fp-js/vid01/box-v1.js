/**
 * Produces the next char based on the numeric input string.
 *
 * @param {string} value The numeric string.
 * @return {string} The computed character.
 *
 * @sig 1String -> 1String
 *
 * ASSUME: The input is a valid numeric value.
 *
 * ASSUME: Client code provides numeric input value which
 * produces the character they want.
 *
 * @example
 * nextCharFromNumStr('64');
 * // → 'A'
 *
 * @example
 * nextCharFromNumStr(' 64  ');
 * // → 'A'
 */
function nextCharFromNumStr(value) {
  const trimmed = value.trim();
  const num = parseInt(trimmed, 10);
  const nextNum = num + 1;
  return String.fromCharCode(nextNum);
}

export { nextCharFromNumStr }
