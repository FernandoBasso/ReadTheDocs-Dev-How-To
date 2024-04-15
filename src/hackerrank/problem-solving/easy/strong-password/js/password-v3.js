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

/**
 * Checks if `str` contains a char from `oneOf`.
 *
 * - T.C: O(n²)
 *
 * @sig String -> String -> Boolean
 * @param {string} str
 * @param {string} oneOf
 * @returns {boolean}
 */
function contains(str, oneOf) {
  return !![...oneOf].find(c => str.includes(c));
}

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
  const charClasses = [NUMS, LOWERS, UPPERS, SPECIALS];

  const missingClasses = charClasses.reduce((count, charClass) => {
    return contains(password, charClass) ? count - 1: count;
  }, charClasses.length);

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
