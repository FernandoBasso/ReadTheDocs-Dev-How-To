/**
 * Returns only the odd numbers from xs.
 *
 * **TIME COMPLEXITY**: `O(n)`. Iterate over every element of the input.
 *
 * **SPACE COMPLEXITY**: `O(n)` (perhaps `O(n log n)`?) because the
 * collected odds potentially increases in direct proportion to the
 * input size.
 *
 * This is my own solution based on things I learned studying HtDP and
 * Lisp/Scheme, purely recursive using an accumulator.
 *
 * @param {Array<number>} xs
 * @return {Array<number>}
 */
function collectOdds(xs, acc = []) {
  if (xs.length === 0) return acc;
  if (xs[0] % 2 !== 0) acc.push(xs[0]);
  return collectOdds(xs.slice(1), acc);
}

export { collectOdds };
