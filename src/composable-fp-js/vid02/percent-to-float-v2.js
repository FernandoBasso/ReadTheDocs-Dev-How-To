import { Box } from '../lib/box';

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
function percentToFloat(percentStr) {
  return Box(percentStr)
    .map(s => s.replace(/\%/g, ''))
    .map(s => Number.parseFloat(s))
    .fold(f => f * 0.01);
}

export { percentToFloat }
