/**
 * Checks whether `sub` is a subsequence of `str`.
 *
 * NOTE: This solution is the one from the instructor. I just renamed
 * some variables in a more meaningful, self-documenting way.
 *
 * **TIME COMPLEXITY**: `O(n + m)`.
 *
 * **SPACE COMPLEXITY**: `O(1)`.
 *
 * This solution is more clever than my previous one because it
 * increments the “pointers” at more specific locations, requiring less
 * code (which is elegant) and also the checks are performed in a more
 * natural order.
 *
 * For instance, my previous solution returns true as the first thing
 * inside the loop and then increments stuff. This solution is more
 * natural — it first checks, increments `idxSub` and then checks if we
 * got the end of `sub`. Also, it does not need an `else` clause.
 *
 * Very well designed solution indeed!
 *
 * @param {string} sub The subsequence to try to find in `str`.
 * @param {string} str The string where the subsequence is to be
 *   searched in.
 * @param {boolean}
 */
function isSubSeq(sub, str) {
  if (sub.length > str.length) return false;

  let idxSub = 0;
  let idxStr = 0;

  while (idxStr < str.length) {
    if (sub[idxSub] === str[idxStr]) ++idxSub;
    if (idxSub === sub.length) return true;
    ++idxStr;
  }

  return false;
}

export { isSubSeq };
