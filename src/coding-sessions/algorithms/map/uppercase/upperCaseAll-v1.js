const { toUpper } = require('./toUpper-v1');

/**
 * Uppercases all strings in the array.
 *
 * @param {string[]} strs
 * @returns {string[]}
 */
function upperCaseAll(strs) {
  return strs.map(toUpper);
}

module.exports = { upperCaseAll };
