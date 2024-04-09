var log = console.log.bind(console);

/**
 * Count the frequencies of negatives, zeros and positive integers.
 *
 * T.C: O(n).
 * S.C: O(1).
 *
 * @sig [Int] -> [Int, Int, Int]
 */
function countFreqs(xs) {
  return xs.reduce(function counter(acc, x) {
    var key = x < 0
      ? 'negatives'
      : x === 0
        ? 'zeroes'
        : 'positives';

    ++acc[key];

    return acc;
  }, { negatives: 0, zeroes: 0, positives: 0 });
}

/**
 * Get the ratios of negatives, zeroes and positives.
 *
 * ASSUME: Array is not empty (can't get ratio of empty array because
 * no division by zero can occur).
 *
 * T.C: O(1).
 * S.C: O(1).
 *
 * @sig [Int] -> [Number, Number, Number]
 */
function getRatios(xs) {
  var len = xs.length;
  var freqs = countFreqs(xs);

  return {
    negatives: freqs.negatives / len,
    zeroes: freqs.zeroes / len,
    positives: freqs.positives / len,
  };
}

function print(xs) {
  var ratios = getRatios(xs);

  log(ratios.positives);
  log(ratios.negatives);
  log(ratios.zeroes);
};

print([-5, 1, 0, -2, 9, 19, 41]);
print([1, 1, 0, -1, -1]);

export {
  countFreqs,
  getRatios,
};
