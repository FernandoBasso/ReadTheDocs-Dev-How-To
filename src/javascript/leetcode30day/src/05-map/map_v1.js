/**
 * Applies a function to each element of the input array and returns
 * a new array with the result.
 *
 * - T.C: O(n). Applies fn once per each element. The final time
 *   complexity will depend on the time complexity of the callback.
 * - S.C: Same notes as for T.C.
 *
 * @param {unknown[]} xs
 * @param {(val: number, idx: number) => unknown[]} fn The index of
 *   the current element is passed to the callback, but it is up
 *   to the callback to decide if it wants to use it or not.
 * @returns {unknown[]}
 */
function map(xs, fn) {
  var mapped = [],
      x,
      i;

  // for (i = 0; i < xs.length; ++i)
  for (i = 0; x = xs[i], i < xs.length; ++i)
    mapped.push(fn(x, i));

  return mapped;
}

export { map };
