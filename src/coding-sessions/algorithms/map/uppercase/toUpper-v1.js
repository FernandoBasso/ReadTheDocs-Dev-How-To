/**
 * Uppercases the entire string.
 *
 * @param {string} str
 * @returns {string}
 *
 * @example
 * toUpper('hello');
 * // → 'Hello'
 *
 * @example
 * toUpper('hello world');
 * // → 'Hello world'
 */
function toUpper(str) {
  return str.toUpperCase();
}

module.exports = { toUpper };
