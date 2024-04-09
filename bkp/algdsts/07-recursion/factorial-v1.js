/**
 * Computes the factorial of x.
 *
 * ASSUME: `x >= 1`.
 *
 * **TIME COMPLEXITY**: `O(n)` because we have to loop for
 * proportionally to the size of `x`.
 *
 * **SPACE COMPLEXITY**: `O(1)` since we only multiply `x` with the next
 * recursive invocation. Not sure how the space complexity is affected
 * by the recursive calls, though.
 *
 * @param {number} x
 * @return {number}
 */
function fact(x) {
  if (x === 0 || x === 1) return 1;
  return x * fact(x - 1);
}

export { fact };
