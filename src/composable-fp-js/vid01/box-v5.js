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
    fold: f => f(val),
    toString: () => `Box(${val})`,
  };
};

/**
 * Produces the next char based on the numeric input string.
 *
 * ASSUME: The input is a valid numeric value.
 *
 * @param {string} value The numeric string.
 * @return {string} The computed character.
 *
 * @example
 * nextCharFromNumStr('64');
 * // → 'A'
 *
 * @example
 * nextCharFromNumStr(' 64  ');
 * // → 'A'
 */
const nextCharFromNumStr = value => {
  return Box(value)
    .map(s => s.trim())
    .map(s => parseInt(s, 10))
    .map(i => i + 1)
    .map(i => String.fromCharCode(i))
    .fold(c => c.toLowerCase());
};

l(nextCharFromNumStr(' 64 '));
l(nextCharFromNumStr(' 96 '));
// → a
// → a

//
// `map' is not supposed to _only_ loop over things. It has to do with
// *composition within a context*. ```Box`` is the context in this case.
// ``Box`` is a “container” type to capture different behaviours.
// ``Box`` is the *identity functor*.
//
// → a
// → a

//
// NOTE: The `inspect' thing used in the video doesn't work in recent
// versions of node (2021, v14 at least). Overriding `toString' should
// work. But then we must make sure we try to log the box as a string to
// trigger the `toString' mechanism.
//

//
// vim: set tw=72:
//
