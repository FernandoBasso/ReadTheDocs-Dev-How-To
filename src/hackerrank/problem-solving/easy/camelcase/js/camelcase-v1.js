/**
 * Counts the number of words in a camelCase string.
 *
 * @param {string} s
 * @return {number}
 */
function camelcase(s) {
  if (s.length === 0) return 0;

  var re = /[A-Z]/,
      cnt = 1,
      i;

  for (i = 0; i < s.length; ++i)
    if (re.test(s[i])) ++cnt;

  return cnt;
}

export { camelcase };
