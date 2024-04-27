/**
 * Applies all functions from left to right. Works as the identity
 * function if `fns` is empty.
 *
 * - T.C: Depends on T.C of input `fns` and `val`.
 * - S.C: Same notes as T.C apply.
 *
 * @param {Array<Function>} fns
 * @returns {(val: unknown) => unknown}
 */
function compose(fns) {
  /**
   * @param {unknown} val
   * @returns {unknown}
   */
  return function composed(val) {
    return fns.reduceRight(function reducer(acc, fn) {
      return fn(acc, val);
    }, val);
  };
}

export { compose };
