/**
 * Sums all numbers from 1 to n.
 *
 * ASSUME: `n >= 0`.
 *
 * Solution from the instructor.
 *
 * @param {number} n
 * @return {number}
 */
function sumRange(n) {
  if (n === 0) return 0;
  return n + sumRange(n - 1);
}

export { sumRange };
