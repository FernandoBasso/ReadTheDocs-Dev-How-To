import {
  numToDigits,
  dropDigits,
 } from "../../../libjs";

const HUNDRED = 'hundred';

/**
 * Returns the first element of an array.
 *
 * @param {unknown[]} xs
 * @returns {unknown}
 */
function head(xs) {
  return xs[0];
}

/**
 * Returns the last element of an array.
 *
 * @param {unknown[]} xs
 * @returns {unknown}
 */
function last(xs) {
  return xs[xs.length - 1];
}

/**
 * Returns the array without the first element.
 *
 * @param {unknown[]} xs
 * @return {unknown[]}
 */
function tail(xs) {
  return xs.slice(1);
}

/**
 * Checks if dividend is divisible by divisor.
 *
 * @param {number} dividend
 * @param {divisor} divisor
 * @returns {boolean}
 */
function isDivisibleBy(dividend, divisor) {
  return dividend % divisor === 0;
}

/**
 * Returns a tuple with the quotient and remainder of
 * integer division.
 *
 * NOTE: This function is inspired by Haskell's `divMod`:
 *
 *   ghci> divMod 10 3
 *   (3,1)
 *
 * Yet, it seems `quotRem` would be a more precise name because we
 * return the quotient and the remainder of integer division (perhaps
 * there is some mathematical concept here that I don't know about,
 * though).
 *
 * @example
 * const [quot, mod] = divMod(13, 10);
 * // → quot is 10, mod is 3.
 *
 * @param {number} dividend
 * @param {number} divisor
 * @returns {number[]} A tuple with the quotient and divisor.
 */
function divMod(dividend, divisor) {
  const mod = dividend % divisor;
  const quot = dividend - mod;
  return [quot, mod];
}

const ONES = {
   0: 'zero',
   1: 'one',
   2: 'two',
   3: 'three',
   4: 'four',
   5: 'five',
   6: 'six',
   7: 'seven',
   8: 'eight',
   9: 'nine',
  10: 'ten',
  11: 'eleven',
  12: 'twelve',
  13: 'thirteen',
  14: 'fourteen',
  15: 'fifteen',
  16: 'sixteen',
  17: 'seventeen',
  18: 'eighteen',
  19: 'nineteen',
};

const TENS = {
  20: 'twenty',
  30: 'thirty',
  40: 'forty',
  50: 'fifty',
  60: 'sixty',
  70: 'seventy',
  80: 'eighty',
  90: 'ninety',
};

/**
 * Converts a number to its written form.
 *
 * @param {number} num
 * @returns {string}
 */
function numToWords(num) {
  const digits = numToDigits(num);

  if (digits.length <= 2) {
    if (num < 19) return ONES[num];

    const [quot, mod] = divMod(num, 10);

    if (mod === 0) return `${TENS[num]}`;

    return `${TENS[quot]} ${ONES[mod]}`;
  }
  else {
    const firstDigit = head(digits);
    const lastTwoDigits = dropDigits(num, digits.length - 2);

    const [quot, mod] = divMod(lastTwoDigits, 10);

    if (isDivisibleBy(num, 100))
      return `${ONES[firstDigit]} ${HUNDRED}`;

    if (last(digits) === 0)
      return `${ONES[firstDigit]} ${HUNDRED} and ${TENS[lastTwoDigits]}`;

    return `${ONES[firstDigit]} ${HUNDRED} and ${TENS[quot]} ${ONES[mod]}`;
  }
}

export { numToWords };
