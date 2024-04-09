/**
 * Calculates ‘b’ raised to the exponent ‘e’.
 *
 * ASSUME: `e` is a natural number.
 *
 * **TIME COMPLEXITY**: `O(n)` because we have to iterate for as many
 * times as is the value of `e`.
 *
 * **SPACE COMPLEXITY**: `O(1)`. We don't require storage that grows
 * proportionally according to the size of the input.
 *
 * This is another way to write the function, that is, using a normal if
 * instead of a ternary conditional.
 *
 * ```js
 * function power(b, e) {
 *   if (e === 0) return 1;
 *   return b * power(b, e - 1);
 * }
 * ```
 *
 * @param {number} b The base number.
 * @param {number} e The exponent.
 * @return {number}
 */
function power(b, e) {
  // if (e === 0) return 1;
  // return b * power(b, e - 1);
  return e === 0 ? 1 : b * power(b, e - 1);
}

export { power };
