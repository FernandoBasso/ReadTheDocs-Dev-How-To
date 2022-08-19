const { isEven } = require('../basic-1/even-odd/isEven-v7');

/**
 * Returns an array of even numbers.
 *
 * Procedural version using helper `isEven()`.
 *
 * @param {number[]} xs
 * @param {number[]}
 */
function filterEven(xs) {
  const evens = [];

  for (let i = 0; i < xs.length; ++i) {
    if (isEven(xs[i])) {
      evens.push(xs[i]);
    }
  }

  return evens;
}

module.exports = { filterEven };
