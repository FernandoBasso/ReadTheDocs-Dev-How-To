const trim = s => s.trim();
const toInt = s => parseInt(s, 10);
const add1 = n => n + 1;
const intToChar = i => String.fromCharCode(i);

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
  return Box(value)
    .map(trim)
    .map(toInt)
    .map(add1)
    .fold(intToChar); /* (1) */
};

export { nextCharFromNumStr }
