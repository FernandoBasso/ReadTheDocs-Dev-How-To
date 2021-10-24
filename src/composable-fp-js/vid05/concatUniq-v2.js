import { fromNullable } from '../lib/index.js';

/**
 * Concatenates the `x` value to `ys` array only if the array does not
 * already contain that `x` value.
 *
 * ASSUME: `x` and the elements of `ys` are primitive values that can be
 * compared with the triple equals operator `===`.
 *
 * @sig a -> [a] -> [a]
 *
 * @param {any} x A value to be appended to `ys`.
 * @param {any[]} = [ys = []] ys A list of values.
 * @return {Array}
 *
 * @example
 * // Does not append 0 to the array because it already contains
 * // 0. That is, it does NOT return `[0, 1, 2, 0]`.
 * concatUniq(0, [0, 1, 2]);
 * // → [0, 1, 2]
 *
 * concatUniq(0, [1, 2]);
 * // → [1, 2, 0]
 */
function concatUniq(x, ys = []) {
  return fromNullable(ys.filter(y => y === x)[0])
    .fold(_ => ys.concat(x), _ => ys);
}

export { concatUniq };
