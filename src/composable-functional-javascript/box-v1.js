const l = console.log.bind(console);

//
// String.fromCharCode(65);
// → 'A'
//

/**
 * Produces the next char based on the numeric input string.
 *
 * @example
 * nextCharFromNumStr('64');
 * // → 'A'
 *
 * @example
 * nextCharFromNumStr(' 64  ');
 * // → 'A'
 *
 * ASSUME: The input is a valid numeric value.
 *
 * @param {string} value The numeric string.
 * @return {string} The computed character.
 */
const nextCharFromNumStr = value => {
  const trimmed = value.trim();
  const num = parseInt(trimmed, 10);
  const nextNum = num + 1;
  return String.fromCharCode(nextNum);
};

//
// CONS:
// • Too many temporary variables.
// • Very prodecural.
//

l(nextCharFromNumStr(' 64'));
l(nextCharFromNumStr(' 96  '));
// → A
// → a

//
// vim: set tw=72:
//
