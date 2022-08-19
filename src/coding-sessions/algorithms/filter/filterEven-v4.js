const { isEven } = require('../basic-1/even-odd/isEven-v7');

/**
 * Returns an array of even numbers.
 *
 * More functional style version using filter and a callback
 * predicate function.
 *
 * @param {number[]} xs
 * @param {number[]}
 */
function filterEven(xs) {
  return xs.filter(function checkIfEven(x) {
    return isEven(x);
  });
}

module.exports = { filterEven };
