/**
 * Sums all numbers from 1 to n.
 *
 * ASSUME: `n >= 0`.
 *
 * This solution neither uses an accumulator parameter, nor does it use
 * a helper function. It uses a local variable to each function, but a
 * somewhat clever logic makes it return the final value correctly.
 *
 * **TIME COMPLEXITY**: `O(n)`
 *
 * **SPACE COMPLEXITY**: `O(1)`.
 *
 * @param {number} n The end point of the range, inclusive.
 * @return {number}
 */
function sumRange(n) {
  let total = 0;

  if (n === 0) return total;

  if (n > 0) total += n;

  total += sumRange(n - 1);

  return total;
}

export { sumRange };
