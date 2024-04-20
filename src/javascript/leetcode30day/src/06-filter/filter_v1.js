/**
 * Returns a new array containing only elements from the original
 * array that satisfy the predicate.
 *
 * - T.C: O(n). Applies `predicate` once per each element. The final
 *   time complexity will depend on the time complexity of the
 *   predicate.
 * - S.C: Same notes as for T.C.
 *
 * @param {unknown[]} xs
 * @param {(val: number, idx: number) => unknown[]} fn The index of
 *   the current element is passed to the predicate, but it is up
 *   to the predicate to decide if it wants to use it or not.
 * @returns {unknown[]}
 */
function filter(xs, predicate) {
  var filtered = [],
    x,
    i;

  for (i = 0; x = xs[i], i < xs.length; ++i)
    if (predicate(x, i))
      filtered.push(x);

  return filtered;
}

export { filter };
