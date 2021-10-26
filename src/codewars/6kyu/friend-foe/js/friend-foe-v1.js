/**
 * @type {number}
 */
const REQUIRED_LENGHT = 4;

/**
 * Checks whether the input string has the length `len`.
 *
 * @sig Number -> String -> Boolean
 *
 * @param {Number}
 * @return {(function(String): Boolean}
 *
 * @example
 * hasLengthOf(4)('ECMA');
 * // → true
 *
 * const hasLengthOf1 = hasLengthOf(1);
 * hasLengthOf1('hey');
 * // → false
 */
const hasLengthOf = len => ({ length }) => length === len;

/** Checks if a given name is a friend.
 *
 * NOTE: Frieds are people who have four-letter names, like “Yoda”,
 * “Leia” or “Luke” 🤣.
 *
 * @sig [String] -> [String]
 *
 * @param {Array<string>} names
 * @return {Array<string>}
 *
 * @example
 * friend(['Ahsoka', 'Luke', 'Aayla', 'Yoda']);
 * // → ['Luke', 'Yoda']
 */
export function friend(names) {
  return names.filter(hasLengthOf(REQUIRED_LENGHT));
};
