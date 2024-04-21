/**
 * Applies a reducing function to `xs` and returns the result.
 * Returns `init` if `xs` is empty.
 *
 * - T.C: O(n), but final T.C will depend on T.C of reducing fn.
 * - S.C: Same notes as T.C.
 *
 * @param {unknown[]} xs
 * @param {(acc: unknown, x: unknown) => unknown} fn
 * @param {unknown} init
 * @returns {unknown}
 */
function reduce(xs, fn, init) {
  var { length: len } = xs,
      acc = init,
      x,
      i;

  for (i = 0; x = xs[i], i < len; ++i)
    acc = fn(acc, x);

  return acc;
}

export { reduce };
