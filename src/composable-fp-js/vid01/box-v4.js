const trim = s => s.trim();
const toInt = s => parseInt(s, 10);
const add1 = n => n + 1;
const intToChar = i => String.fromCharCode(i);

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
  return [value].map(trim)
                .map(toInt)
                .map(add1)
                .map(intToChar)[0]; /* <1> */
};

export { nextCharFromNumStr }
