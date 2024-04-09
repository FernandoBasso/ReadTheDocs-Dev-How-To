/**
 * Returns the subtotals of the sum of the array elements.
 *
 * @param {Array<number>} xs
 * @return {Array<number>}
 *
 * **TIME COMPLEXITY**: `O(n²)`. We have to do a loop inside a loop, and
 * the inner loop eventually runs for the entire length of the input
 * array.
 *
 * **SPACE COMPLEXITY**: `O(n)`. The result array increases as the input
 * array gets larger.
 *
 * A subtotal is the sum of the first element of the array up to each
 * next element in turn. For example, the array `[1, 2, 3, 4]` has these
 * subtotals:
 *
 * ```plain
 * 1 → sum of just the first element.
 * 3 → sum of 1 and 2.
 * 6 → sum of 1, 2 and 3.
 * 10 → sum of 1, 2, 3 and 4.
 * ```
 *
 * So, `[1, 2, 3]` produces `[1, 3, 6]
 */
function subtotals(xs) {
  const result = [];

  for (let i = 0; i < xs.length; ++i) {
    let subtotal = 0;

    for (let j = 0; j <= i; ++j) {
      subtotal += xs[j];
    }

    result.push(subtotal);
  }

  return result;
}

export { subtotals };
