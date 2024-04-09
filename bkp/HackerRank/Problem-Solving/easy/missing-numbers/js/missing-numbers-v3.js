/**
 * Compares two numbers for ascending sorting.
 *
 * @param {number} a
 * @param {number} b
 * @returns {number}
 */
function sortAsc(a, b) {
  return a - b;
}

/**
 * Finds numbers that are present in `brr` but missing in `arr`.
 *
 * @param {number[]} arr
 * @param {number[]} brr
 * @returns {number[]} The array of the missing numbers (the
 *   difference).
 */
function missingNums(arr, brr) {
  for (const n of arr) {
    if (brr.includes(n)) {
      const idx = brr.indexOf(n);
      brr.splice(idx, 1);
    }
  }

  //
  // • Use sets to remove duplicates.
  // • Convert back to array with spread syntax.
  //
  return [...new Set(brr)].sort(sortAsc);
}

export { missingNums };
