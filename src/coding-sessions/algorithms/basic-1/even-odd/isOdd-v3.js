const { isEven } = require('./isEven-v7');

/**
 * Checks if `n` is an odd number.
 *
 * `isOdd()` is defined in terms of `isEven()`.
 *
 * @param {number} n The number to check.
 * @returns {boolean}
 */
function isOdd(n) {
  return !isEven(n);
}

module.exports = { isOdd };
