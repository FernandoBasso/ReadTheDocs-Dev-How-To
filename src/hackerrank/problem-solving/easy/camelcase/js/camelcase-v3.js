/**
 * Counts the number of words in a camelCase string.
 *
 * - T.C: O(n).
 * - S.C: O(1).
 *
 * @param {string} s
 * @return {number}
 */
function camelcase(s) {
  if (s.length === 0) return 0;

  return s.split(/[A-Z]/).length;
}

export { camelcase };
