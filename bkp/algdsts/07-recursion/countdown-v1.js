/**
 * Logs number from `num` to 0.
 *
 * ASSUME: `num >= 0`.
 *
 * **TIME COMPLEXITY**: `O(n)` since we have to consume the numbers all
 * the way from `num` to 0.
 *
 * **SPACE COMPLEXITY**: `O(1)` since we do not store any array or
 * object that grows as input grows. Not sure how the recursive stack
 * affects the interpretation of space complexity, though.
 *
 * @param {number} num
 */
function countdown(num) {
  if (num <= 0) {
    console.info("All done!");
    return;
  }

  console.log(num);
  return countdown(--num);
}

export { countdown };
