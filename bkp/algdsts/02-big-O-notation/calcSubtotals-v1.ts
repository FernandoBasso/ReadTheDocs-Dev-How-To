/**
 * Returns the subtotals of the sum of the array elements.
 *
 * @param xs The input array of numbers.
 * @return The array of subtotals.
 *
 * **TIME COMPLEXITY**: `O(n²)`. We have to do a loop inside a loop, and
 * the inner loop eventually runs for the entire length of the input
 * array.
 *
 * **SPACE COMPLEXITY**: `O(n)`. The result array increases as the input
 * array gets larger. For every element in `xs`, we create a new element
 * in the resulting array.
 *
 * A subtotal is the sum of the first element of the array up to each
 * next element in turn. For example, the array `[1, 2, 3, 4]` has these
 * subtotals:
 *
 * ```
 *  1 → sum of just the first element.
 *  3 → sum of 1 and 2.
 *  6 → sum of 1, 2 and 3.
 * 10 → sum of 1, 2, 3 and 4.
 * ```
 *
 * So, for example, the sum of `[1, 2, 3]` produces `[1, 3, 6].
 */
function calcSubtotals(xs: number[]): number[] {
  const subtotals = [];

  for (let i = 0; i < xs.length; ++i) {
    let subtotal = 0;

    for (let j = 0; j <= i; ++j) {
      subtotal += xs[j];
    }

    subtotals.push(subtotal);
  }

  return subtotals;
}

export { calcSubtotals };
