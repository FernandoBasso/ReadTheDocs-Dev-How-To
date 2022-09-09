/**
 * Sums the numbers of the array of numbers.
 *
 * @param {number[]} xs
 * @returns {number}
 * @example
 * sum([1, 2, 3]);
 * // → 6
 */
function sum(xs) {
  let total = 0;

  for (let i = 0; i < xs.length; i += 1) {
    total = total + xs[i];
  }

  return total;
}

module.exports = { sum };
