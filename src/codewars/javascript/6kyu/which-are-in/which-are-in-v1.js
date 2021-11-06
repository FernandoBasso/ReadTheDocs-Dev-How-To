/**
 * Retruns a lexicographically sorted list of strings in `xs` which
 * are substrings of the strings in `ys`.
 *
 * ASSUME: parameters are valid lists of strings.
 *
 * @signature
 * [String] [String] -> [String]
 *
 * @param {Array<string>} xs
 * @param {Array<string>} ys
 * @return {Array<string>}
 *
 *  @example
 *  inArray(['hi'], ['good-bye']);
 *  // → []
 *
 *  inArray(['live'], ['Portal' 'Still' 'Alive']);
 *  // → ['live']
 */
function inArray(xs, ys) {
  return xs.filter(function f (x) {
    return ys.find(function g (y) {
      return y.includes(x);
    });
  }).sort();
}

export { inArray };
