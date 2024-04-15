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
  const missingLength = MIN_LEN - length;
  const charClasses = [NUMS, LOWERS, UPPERS, SPECIALS];

  const missingType = charClasses.reduce((count, charClass) => {
    return contains(password, charClass) ? count - 1 : count;
  }, charClasses.length);

  return Math.max(missingLength, missingType);
}

export { minNum };
