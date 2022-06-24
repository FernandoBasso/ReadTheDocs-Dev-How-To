/**
 * Sums all numbers from 1 to n.
 *
 * ASSUME: `n >= 0`.
 *
 * This solution uses an accumulator parameter to store the value
 * computed and in each invocation.
 *
 * **TIME COMPLEXITY**: `O(n)`
 *
 * **SPACE COMPLEXITY**: `O(1)`.
 *
 * @param {number} n The end point of the range, inclusive.
 * @param {number} [acc = 0] The value to be used as an accumulator.
 *  Users should not directly use this variable, if they choose to do
 *  so, it must be passed 0. Client code can pass a value other than
 *  zero if they need to add a sum from 0 to n to an already existing
 *  value.
 * @return {number}
 */
function sumRange(n, acc = 0) {
  if (n === 0) return acc;
  return n + sumRange(n - 1, acc);
}

export { sumRange };
