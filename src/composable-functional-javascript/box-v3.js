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
  return [value].map(s => s.trim())
                .map(s => parseInt(s, 10))
                .map(i => i + 1)
                .map(i => String.fromCharCode(i));
};

//
// This example use the concept of a “box” to map over values.
//
// PROS:
// • Easier to read the sequence of things that happen.
// • Easy to add new operaions any where in the chain.
//


l(nextCharFromNumStr('64'));
l(nextCharFromNumStr('96'));
// → ['A']
// → ['a']

//
// vim: set tw=72:
//
