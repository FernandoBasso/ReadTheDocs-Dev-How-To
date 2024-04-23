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
    var lastFnIdx = fns.length - 1,
        result = val,
        i;

    for (i = lastFnIdx; i >= 0; --i)
      result = fns[i](result);

    return result;
  };
}

export { compose };
