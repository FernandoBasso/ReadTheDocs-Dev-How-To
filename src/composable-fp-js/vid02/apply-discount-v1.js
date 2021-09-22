import { moneyToFloat } from './money-to-float-v2';
import { percentToFloat } from './percent-to-float-v2';

/**
 * Applies a dicount to the given price.
 *
 * ASSUME: price and discount are proper price and discount strings,
 * e.g: '$49.99' and '20%'.
 *
 * @sig String String -> Number
 *
 * @param {string} price
 * @param {string} discount
 * @return {number}
 *
 * @example
 * applyDiscount('$100', '20%');
 * // → 80
 *
 * @example
 * applyDiscount('$50.00', '5%');
 * // → 47.5
 */
function applyDiscount(price, discount) {
  const cost = moneyToFloat(price);
  const savings = percentToFloat(discount);
  return cost - cost * savings;
}

export { applyDiscount }
