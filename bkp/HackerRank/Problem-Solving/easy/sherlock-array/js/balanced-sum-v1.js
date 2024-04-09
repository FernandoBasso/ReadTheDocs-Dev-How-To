// @ts-check

//
// This version is correct does not satisfy the time constraints in
// HackerRank. Execution times out.
//

/**
 * Gets the elements to the left of `idx`.
 *
 * @param {number} idx
 * @param {Array<number>} xs
 * @returns {Array<number>}
 */
function left(idx, xs) {
  return xs.slice(0, idx);
}

/**
 * Gets the elements to the right of `idx`.
 *
 * @param {number} idx
 * @param {Array<number>} xs
 * @returns {Array<number>}
 */
function right(idx, xs) {
  return xs.slice(idx + 1);
}

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
      rSum = 0;

  for (p = 0; p < xs.length; ++p) {
    lSum = sum(left(p, xs));
    rSum = sum(right(p, xs));

    if (lSum === rSum) return 'YES';
  }

  return 'NO';
}

export { balancedSum };
