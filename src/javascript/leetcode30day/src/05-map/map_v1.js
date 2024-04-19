/**
 * Applies a function to each element of the input array and returns
 * a new array with the result.
 *
 * @param {unknown[]} xs
 * @param {(v: unknown) => unknown} fn
 */
function map(xs, fn) {
  var mapped = [];

  for (var i = 0; i < xs.length; ++i)
    mapped.push(fn(xs[i]));

  return mapped;
}

export { map };
