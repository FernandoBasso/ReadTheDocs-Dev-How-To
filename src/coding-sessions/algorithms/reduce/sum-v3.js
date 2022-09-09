/**
 * Adds two numbers.
 *
 * @param {number} x
 * @param {number} y
 * @returns {number}
 */
function add(acc, num) {
  return acc + num;
}

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
  return xs.reduce(add, 0);
}

module.exports = { sum };

