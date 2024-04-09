/**
 * Sum natural numbers from 1 to x.
 *
 * **TIME COMPLEXITY**: `O(n)`. We have to keep adding as long as we
 * reach the end of the range.
 *
 * **SPACE COMPLEXITY**: `O(1)`. We just keep adding to the parameter
 * `x`. Not sure about how the call stack should be interpreted when it
 * comes to space complexity.
 *
 * ASSUME: `x >= 1`.
 *
 * @param {number} x
 * @return {number}
 */
function sumFrom1To(x) {
  if (x === 1) return 1;
  return x + sumFrom1To(x - 1);
}

export { sumFrom1To };
