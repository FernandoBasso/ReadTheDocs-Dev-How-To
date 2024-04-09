/**
 * Finds numbers that are present in `brr` but missing in `arr`.
 *
 * @param {number[]} arr
 * @param {number[]} brr
 * @returns {number[]} The array of the missing numbers (the
 *   difference).
 */
function missingNums(arr, brr) {
  var freqsArr = {},
      freqsBrr = {},
      i,
      n;

  for (i = 0; n = arr[i], i < arr.length; ++i)
    freqsArr[n] = freqsArr[n] + 1 || 1;

  for (i = 0; n = brr[i], i < brr.length; ++i)
    freqsBrr[n] = freqsBrr[n] + 1 || 1;

  return Object.keys(freqsBrr).reduce((missing, key) => {
    if (freqsBrr[key] === freqsArr[key])
      return missing;

    missing.push(Number(key));

    return missing;
  }, []);
}

export { missingNums };
