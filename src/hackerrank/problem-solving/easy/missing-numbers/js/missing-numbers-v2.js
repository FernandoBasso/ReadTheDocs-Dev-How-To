/**
 * Returns a hash map of the frequencies of the values in `xs`.
 *
 * - T.C: O(n).
 * - S.C: O(n).
 *
 * @param {number[]} xs
 * @returns {{ [key: string]: number }}
 */
function countFreq(xs) {
  return xs.reduce(function reducer(freqs, x) {
    freqs[x] = freqs[x] + 1 || 1;
    return freqs;
  }, {});
}

/**
 * Finds numbers that are present in `ys` but missing in `xs`.
 *
 * - T.C: O(n).
 * - S.C: O(n).
 *
 * @param {number[]} xs
 * @param {number[]} ys
 * @returns {number[]} The array of the missing numbers (the
 *   difference).
 */
function missingNums(xs, ys) {
  var freqXs = countFreq(xs);
  var freqYs = countFreq(ys);

  return Object.keys(freqYs).reduce((missing, key) => {
    if (freqYs[key] === freqXs[key]) return missing;

    return [...missing, Number(key)];
  }, []);
}

export { missingNums };
