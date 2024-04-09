/**
 * Returns only the odd numbers from xs.
 *
 * **TIME COMPLEXITY**: `O(n)`. Iterate over every element of the input.
 *
 * **SPACE COMPLEXITY**: `O(n)` (perhaps `O(n log n)`?) because the
 * collected odds potentially increases in direct proportion to the
 * input size.
 *
 * Solution is not purely recursive. Uses a helper method.
 *
 * @param {Array<number>} xs
 * @return {Array<number>}
 */
function collectOdds(xs) {
  const odds = [];

  (function go(nums) {
    if (nums.length === 0) return;
    if (nums[0] % 2 !== 0) {
      odds.push(nums[0]);
      go(nums.slice(1));
    }
  })(xs);

  return odds;
}

export { collectOdds };
