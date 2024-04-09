/**
 * A password must contain at least 6 characters.
 */
const MIN_LEN = 6;

/**
 * A password must contain at least 1 char from each of the
 * four character classes blow.
 */
const NUMS = '0123456789';
const LOWERS = 'abcdefghijklmnopqrstuvwxyz';
const UPPERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
const SPECIALS = '!@#$%^&*()-+';

const reDigits = new RegExp(NUMS.split('').join('|'));
const reLowers = new RegExp(LOWERS.split('').join('|'));
const reUppers = new RegExp(UPPERS.split('').join('|'));
const reSpecials = /!|@|#|\$|%|\^|&|\*|\(|\)|-|\+/;

/**
 * Returns the number of missing chars required to satisfy the password
 * requirements.
 *
 * @sig Number -> String -> Number
 * @param {number} length The length of the input password string.
 * @param {string} passowrd The password string.
 * @returns {number} The number of missing chars.
 */
function minNum(length, password) {
  const regexps = [reLowers, reUppers, reDigits, reSpecials];

  const missingClasses = regexps.reduce((count, re) => {
    return re.test(password) ? count - 1 : count;
  }, regexps.length);

  return Math.max(MIN_LEN - length, missingClasses);
}

/*

minNum(0, ''))
minNum(5, 'llUU%'))
minNum(6, 'llUU%%'))
minNum(5, 'llUU9'))
minNum(6, 'llUU99'))
minNum(1, 'z'))
minNum(1, 'Z'))
minNum(1, '9'))
minNum(1, '%'))
minNum(6, 'zzzzzz'))
minNum(6, 's3cR%t'))
minNum(7, 'lUUU999'))

*/

export { minNum };
