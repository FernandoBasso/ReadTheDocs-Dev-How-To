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
 * This function allows us to chain map() invocations in a composable
 * way, and, if desired, unbox the value using fold().
 *
 * @sig Value -> Container
 *
 * @param {Value} val
 * @return {Container}
 *
 * @example
 * Box('YODA')
 *   .map(s => s.toLowerCase())
 *   .map(s => s.split(''))
 *   .fold(s => s.join('-'))
 * // → 'y-o-d-a'
 */
function Box(value) {
  return {
    map: f => Box(f(value)),
    fold: f => f(value),
    toString: () => `Box(${value})`,
  };
}

export { Box }
