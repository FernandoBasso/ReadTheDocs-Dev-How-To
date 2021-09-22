/**
 * Parses a USD monetory string to its float numeric representation.
 *
 * ASSUME: The input is in the proper USD monetory format, e.g '$2.99'.
 *
 * @param {string} s The USD currency.
 * @return {number} The parsed floatish value.
 *
 * @sig String -> Number
 *
 * @example
 *
 * moneyToFloat('$1.10');
 * // → 1.1
 *
 * moneyToFloat('$4.99');
 * // → 4.99
 */
function moneyToFloat(s) {
  return parseFloat(s.replace(/\$/g, ''));
}

export { moneyToFloat }
