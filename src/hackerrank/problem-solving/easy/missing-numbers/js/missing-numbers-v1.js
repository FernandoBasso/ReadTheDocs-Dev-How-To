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
function missingNums(arr, brr) {
  var freqXs = {},
      freqYs = {},
      i,
      n;

  for (i = 0; n = xs[i], i < xs.length; ++i)
    freqXs[n] = freqXs[n] + 1 || 1;

  for (i = 0; n = ys[i], i < ys.length; ++i)
    freqYs[n] = freqYs[n] + 1 || 1;

  return Object.keys(freqYs).reduce((missing, key) => {
    if (freqYs[key] === freqXs[key])
      return missing;

    missing.push(Number(key));

    return missing;
  }, []);
}

export { missingNums };
