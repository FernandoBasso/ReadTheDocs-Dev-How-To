/**
 * Compares two numbers for ascending sorting.
 *
 * - T.C: O(1).
 * - S.C: O(1).
 *
 * @param {number} a
 * @param {number} b
 * @returns {number}
 */
function sortAsc(a, b) {
  return a - b;
}

/**
 * Finds numbers that are present in `ys` but missing in `xs`.
 *
 * - T.C: O(n).
 * - S.C: O(1).
 *
 * @param {number[]} xs
 * @param {number[]} ys
 * @returns {number[]} The array of the missing numbers (the
 *   difference).
 */
function missingNums(xs, ys) {
  for (const n of xs) {
    if (ys.includes(n)) {
      const idx = ys.indexOf(n);
      ys.splice(idx, 1);
    }
  }

  //
  // • Use sets to remove duplicates.
  // • Convert back to array with spread syntax.
  //
  return [...new Set(ys)].sort(sortAsc);
}

export { missingNums };
