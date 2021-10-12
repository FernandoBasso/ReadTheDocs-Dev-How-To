/// <reference path="./typedefs.js" />

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
 * @param {Value} value
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
    chain: _ => Left(value),
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
 * @param {Value} value
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
    chain: f => f(value),
    map: f => Right(f(value)),
    fold: (_, rightFn) => rightFn(value),
    toString: () => `Right(${value})`,
  };
}

export {
  Right,
  Left,
}

