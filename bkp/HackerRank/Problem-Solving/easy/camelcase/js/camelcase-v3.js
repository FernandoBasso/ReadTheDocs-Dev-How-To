/**
 * Counts the number of words in a camelCase string.
 *
 * @param {string} s
 * @return {number}
 */
function camelcase(s) {
  if (s.length === 0) return 0;

  return s.split(/[A-Z]/).length;
}

export { camelcase };
