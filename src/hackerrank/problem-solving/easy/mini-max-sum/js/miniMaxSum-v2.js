/**
 * Computes the addition `x` and `y`.
 *
 * - T.C: O(1).
 * - S.C: O(1).
 *
 * @sig Number Number -> Number
 * @param {number} x
 * @param {number} y
 * @returns {number}
 */
function add(x, y) {
  return x + y;
}

/**
 * Finds the min and max sum of the five-integer array.
 *
 * ASSUME: The input always contains five positive integers and is
 * sorted in ascending order.
 *
 * - T.C: O(n²).
 * - S.C: O(n).
 *
 * @sig [Int] -> { min: Int, max: Int }
 * @param {number} xs
 * @returns {{ min: number, max: number }}
 */
function miniMaxSum(xs) {
  var smallerXs = xs.slice(0, 4);
  var largerXs = xs.slice(0, 4);
  var rest = xs.slice(4);

  var i, max, min, cur;

  for (i = 0; i < rest.length; ++i) {
    cur = rest[i];
    max = Math.max(...smallerXs);
    min = Math.min(...largerXs);

    if (cur < max)
      smallerXs[i] = cur;

    if (cur > min)
      largerXs[i] = cur;
  }

  return {
    min: smallerXs.reduce(add, 0),
    max: largerXs.reduce(add, 0),
  };
}

export { miniMaxSum };
