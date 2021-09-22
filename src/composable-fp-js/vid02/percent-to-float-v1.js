/**
 * Parses a percent string into its numeric value.
 *
 * ASSUME: The input is a valid percent string like '50%'.
 *
 * @param {string} s The percentage
 * @return {number} The parsed floatish value.
 *
 * @sig String -> Number
 *
 * @example
 *
 * percentToFloat('20%');
 * // → 0.2
 *
 * percentToFloat('5%');
 * // → 0.05
 */
function percentToFloat(s) {
  const replaced = s.replace(/\%/g, '');
  const number = Number.parseFloat(replaced);
  return number * 0.01;
}

export { percentToFloat }
