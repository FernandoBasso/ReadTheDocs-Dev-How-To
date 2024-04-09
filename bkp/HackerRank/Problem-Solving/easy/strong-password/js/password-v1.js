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
 * @param {number} length The length of the input password string.
 * @param {string} password The password string.
 * @returns {number} The number of missing chars.
 */
function minNum(length, password) {
  const missingLength = MIN_LEN - length;
  let missingType = 4;

  if (contains(password, NUMS))
    missingType -= 1;

  if (contains(password, LOWERS))
    missingType -= 1;

  if (contains(password, UPPERS))
    missingType -= 1;

  if (contains(password, SPECIALS))
    missingType -= 1;

  return Math.max(missingLength, missingType);
}

export { minNum };
