/**
 * Returns only the odd numbers from xs.
 *
 * **TIME COMPLEXITY**: `O(n)`. Iterate over every element of the input.
 *
 * **SPACE COMPLEXITY**: `O(n)` (perhaps `O(n log n)`?) because the
 * collected odds potentially increases in direct proportion to the
 * input size.
 *
 * This solution is from the instructor. No helper function or extra
 * accumulator parameter is needed.
 *
 * @param {Array<number>} xs
 * @return {Array<number>}
 */
function odds(xs) {
  let arr = [];

  if (xs.length === 0) return arr;

  if (xs[0] % 2 !== 0) arr.push(xs[0]);

  // Concatenate all the arrays “at the end”.
  arr = arr.concat(odds(xs.slice(1)));

  return arr;
}

export { odds }
