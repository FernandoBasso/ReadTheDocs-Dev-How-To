/**
 * A Value consumed and produced by `Right` or `Left` containers.
 *
 * @typedef {any} Value
 */

/**
 * @typedef {Object} RightContainer
 * @property {function(function(Value): Value): Container} map Maps a
 *   function over the value.
 * @property {function(function(Value): Value, function(Value): Value): LeftContainer} fold
 *   Apply the right function over the value and ignores the left function.
 * @property {function(): string} toString
 */

/**
 * @typedef {Object} LeftContainer
 * @property {function(function(Value): Value): LeftContainer} map Maps a
 *   function over the value.
 * @property {function(function(Value): Value, function(Value): Value): LeftContainer} fold
 *   Apply the left function over the value and ignores the right function.
 * @property {function(): string} toString
 */

/**
 * Creates a chainable, `LeftContainer` container.
 *
 * This function allows the chaining `map()` invocations in a composable
 * way, and, if desired, unbox the value using `fold()`.
 *
 * However, `Left` is supposed to handle failures, and therefore,
 * `Left().map()` DOES NOT actually apply the function to the value.
 * It simply returns another `LeftContainer` without modifying the
 * value.
 *
 * @sig Value -> LeftContainer
 *
 * @param {Value} val
 * @return {LeftContainer}
 *
 * @example
 * Left('YODA')
 *   .map(s => s.toLowerCase())
 *   .map(s => s.split(''))
 *   .fold(_ => 'error', s => s.join('-'))
 * // → 'error'
 */
function Left(value) {
  return {
    map: _ => Left(value),
    fold: (leftFn, _) => leftFn(value),
    toString: () => `Left(${value})`,
  };
}

/**
 * Creates a chainable `RightContainer` container.
 *
 * This function allows the chaining of `map()` invocations in a composable
 * way, and, if desired, unbox the value using `fold()`.
 *
 * @sig Value -> RightContainer
 *
 * @param {Value} val
 * @return {RightContainer}
 *
 * @example
 * RightContainer('YODA')
 *   .map(s => s.toLowerCase())
 *   .map(s => s.split(''))
 *   .fold(_ => 'error', s => s.join('-'))
 * // → 'y-o-d-a'
 */
function Right(value) {
  return {
    map: f => Right(f(value)),
    fold: (_, rightFn) => rightFn(value),
    toString: () => `Right(${value})`,
  };
}

export {
  Right,
  Left,
}

