// @ts-check

/**
 * Sums all the numbers in `xs`.
 *
 * @param {Array<number>} xs
 * @returns {number}
 */
function sum(xs) {
  return xs.reduce((total, x) => total + x, 0);
}

/**
 * Checks if there is an element whose sum of all elements on its left
 * is equal to the sum of all elements to its right.
 *
 * @param {Array<number>} xs
 * @returns {'YES' | 'NO'}
 */
function balancedSum(xs) {
  var p,
      lSum = 0,
      rSum = sum(xs.slice(1));

  if (rSum === 0) return 'YES';

  for (p = 1; p < xs.length; ++p) {
    lSum += xs[p - 1];
    rSum -= xs[p];

    if (lSum === rSum) return 'YES';
  }

  return 'NO';
}

export { balancedSum };
