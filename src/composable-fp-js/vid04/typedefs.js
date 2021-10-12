/**
 * @typedef {any} Value
 *
 * A value consumed and/or produced by a container's `map` and/or `fold`
 * functions.
 */

/**
 * @typedef {function(function(Value): Value): LeftContainer} LeftMapFn
 *
 * @sig (Value -> Value) -> LeftContainer
 *
 * `LeftMapFn` is a function that takes a callback function and returns
 * a `LeftContainer`. The callback takes a `Value` and returs a `Value`.
 */

/**
 * @typedef {function(function(Value): Value): RightContainer} RightMapFn
 *
 * @sig (Value -> Value) -> RightContainer
 *
 * `RightMapFn` is a function that takes a callback function and returns
 * a `RightContainer`. The callback takes a `Value` and returs a `Value`.
 * The
 */

/**
 * @typedef {function(function(Value): Value): Value} LeftFoldFn
 *
 * @sig (Value -> Value) -> Value
 *
 * `LeftFoldFn` is a function that takes a callback function and returns
 * a `Value`. The callback takes a `Value` and returs a `Value`.
 */

/**
 * @typedef {function(function(Value): Value): Value} RightFoldFn
 *
 * @sig (Value -> Value) -> Value
 *
 * `RightFoldFn` is a function that takes a callback function and returns
 * a `Value`. The callback takes a `Value` and returs a `Value`.
 */

/**
 * @typedef {Object} LeftContainer
 * @property {LeftMapFn} map This DOES NOT apply the function to the value.
 *   `map()` on a left container refuses to apply the function we do not have a
 *   value, but some sort of error instead.
 * @property {LeftFoldFn} fold  Applies the left function over the value
 *   and ignores the right function.
 * @property {function(): string} toString
 */

/**
 * @typedef {Object} RightContainer
 * @property {RightMapFn} map Maps a function over the value.
 * @property {RightFoldFn} fold Applies the right function over the
 *   value and ignores the left function.
 * @property {function(): string} toString
 */

/**
 * This is a type like `Maybe` in Haskell. It indicates that we
 * either have a result (`Right`) or a failure of some sort (`Left`).
 *
 * @typedef {LeftContainer|RightContainer} Either
 */
