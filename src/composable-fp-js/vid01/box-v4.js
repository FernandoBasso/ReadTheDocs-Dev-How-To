const l = console.log.bind(console);

/**
 * A Value consumed and produced by the Container `map's function.
 *
 * @typedef {any} Value
 */

/**
 * @typedef {Object} Container
 * @property {function(function(Value): Value): Container} map Map a
 *   function over the Value.
 * @property {function(): string} toString Our custom stringification
 *   of the object.
 */

/**
 * Creates a chainable container.
 *
 * @param {Value} val
 * @return {Container}
 */
const Box = val => {
  return {
    map: f => Box(f(val)),
    toString: () => `Box(${val})`,
  };
};

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
  return Box(value)
    .map(s => s.trim())
    .map(s => parseInt(s, 10))
    .map(i => i + 1)
    .map(i => String.fromCharCode(i))
    .map(c => c.toLowerCase());
};

//
// This example use the concept of a “box” to map over values.
//
// PROS:
// • Easier to read the sequence of things that happen.
// • Easy to add new operations anywhere in the chain.
// • We can unify method invocations `s.trim()', function invocations
//   `parseInt(...)', operators  `1 + 1', and qualified invocations
//   `String.fromCharCode()'
//

const result1 = nextCharFromNumStr(' 64 ');
const result2 = nextCharFromNumStr(' 96 ');

l(result1 + '');
l(String(result2));
// → Box(a)
// → Box(a)

//
// NOTE: The `inspect' thing used in the video doesn't work in recent
// versions of node (2021, v14 at least). Overriding `toString' should
// work. But then we must make sure we try to log the box as a string to
// trigger the `toString' mechanism.
//

//
// vim: set tw=72:
//
