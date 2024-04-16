const log = console.log.bind(console);

/**
 * Get the ratios of negatives, zeroes and positives.
 *
 * ASSUME: Array is not empty.
 *
 * T.C: O(n).
 * S.C: O(n).
 *
 * @sig [Int] -> Void
 */
function getRatios(xs) {
  var len = xs.length;

  var freqs = xs.reduce(function reducer(acc, x) {
    var key = x < 0 ? 'n' : x === 0 ? 'z' : 'p';
    ++acc[key];
    return acc;
  }, { n: 0, z: 0, p: 0 });

  ['p', 'n', 'z'].forEach(k => log(freqs[k] / len));
}

getRatios([-5, 1, 0, -2, 9, 19, 41]);
getRatios([1, 1, 0, -1, -1]);
