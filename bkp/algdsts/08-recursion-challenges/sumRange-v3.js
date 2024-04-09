/**
 * Sums all numbers from 1 to n.
 *
 * ASSUME: `n >= 0`.
 *
 * This solution uses a helper function to recursively compute the sum
 * of the range of values.
 *
 * **TIME COMPLEXITY**: `O(n)`
 *
 * **SPACE COMPLEXITY**: `O(1)`.
 *
 * @param {number} n
 * @return {number}
 */
function sumRange(n) {
  let total = 0;

  (function run(x) {
    if (x === 0) return total;
    total += x;
    run(x - 1);
  })(n);

  return total;
}

export { sumRange };
