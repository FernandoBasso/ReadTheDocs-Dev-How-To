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

  var AZ = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';

  var cnt = 1,
      i;

  for (i = 0; i < s.length; ++i)
    if (AZ.includes(s[i])) ++cnt;

  return cnt;
}

export { camelcase };
