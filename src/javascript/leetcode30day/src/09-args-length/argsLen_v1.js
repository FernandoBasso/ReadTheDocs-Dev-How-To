/**
 * Returns the number of arguments passed.
 *
 * - T.C: O(1).
 * - S.C: O(1).
 *
 * @param {...unknown} args
 * @returns {number}
 */
function argsLen(...args) {
  return args.length;
}

export { argsLen };
