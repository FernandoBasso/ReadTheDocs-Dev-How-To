const head = xs => xs[0];
const tail = xs => xs.slice(1);
const isEmpty = xs => xs.length === 0;

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
  return (function go(acc, elems) {
    return isEmpty(elems)
      ? acc
      : go(fn(acc, head(elems)), tail(elems));
  }(init, xs));
}

export { reduce };
