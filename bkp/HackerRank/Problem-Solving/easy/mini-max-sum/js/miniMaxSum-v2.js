/**
 * Computes the addition `x` and `y`.
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
 * @sig [Int] -> { min: Int, max: Int }
 * @param {number} xs
 * @returns {{ min: number, max: number }}
 */
function miniMaxSum(xs) {
  var smalls = xs.slice(0, 4);
  var largers = xs.slice(0, 4);
  var rest = xs.slice(4);

  var i, max, min, cur;

  for (i = 0; i < rest.length; ++i) {
    cur = rest[i];
    max = Math.max(...smalls);
    min = Math.min(...largers);

    if (cur < max)
      smalls[i] = cur;

    if (cur > min)
      largers[i] = cur;
  }

  return {
    min: smalls.reduce(add, 0),
    max: largers.reduce(add, 0),
  };
}

export { miniMaxSum };
