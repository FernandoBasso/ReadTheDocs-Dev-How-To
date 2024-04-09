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

const HUNDRED = 'hundred';

/**
 * Converts a number to its written form.
 *
 * @param {number} num
 * @returns {string}
 */
function numToWords(num) {
  const digits = num.toString().split('');

  if ([1, 2].includes(digits.length)) {
    if (num < 19) return ONES[num];

    const rem = num % 10;
    const quot = num - rem;

    if (rem === 0) return TENS[quot];

    return `${TENS[quot]} ${ONES[rem]}`;
  }
  else {
    const firstDigit = digits[0];
    const lastTwoDigits = digits.slice(1).join('');

    const rem = lastTwoDigits % 10;
    const quot = lastTwoDigits - rem;

    if (num % 100 === 0) return `${ONES[firstDigit]} ${HUNDRED}`;

    if (digits[digits.length - 1] === '0')
      return `${ONES[firstDigit]} ${HUNDRED} and ${TENS[lastTwoDigits]}`;

    return `${ONES[firstDigit]} ${HUNDRED} and ${TENS[quot]} ${ONES[rem]}`;
  }
}

module.exports = { numToWords };
