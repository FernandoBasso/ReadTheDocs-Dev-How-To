/**
 * Returns the number of arguments passed.
 *
 * - T.C: O(1).
 * - S.C: O(1).
 *
 * @param {...unknown} args
 * @returns {number}
 */
function argsLen(...{ length }) {
  return length;
}

export { argsLen };

//
// This is just to show we can destructure `length`.
//
