
/**
 * Returns the index of 0 or 1 to be incremented or `undefined`
 * if there is a tie.
 *
 * @param {number} aliceRating
 * @param {number} bobRating
 * @returns {0|1|undefined}
 */
function whichIndex(aliceRating, bobRating) {
  if (aliceRating > bobRating) return 0;
  else if (bobRating > aliceRating) return 1;
  return undefined;
}

/**
 * Computes a tuple with alice and bob's points.
 *
 * @param {number[]} aliceRatings
 * @param {number[]} bobRatings
 * @returns {number[]}
 */
function compareTriplets(aliceRatings, bobRatings) {
  return aliceRatings.reduce((acc, aliceRating, idx) => {
    const bobRating = bobRatings[idx];
    const idxToIncrement = whichIndex(aliceRating, bobRating);

    if (idxToIncrement === undefined) return acc;

    acc[idxToIncrement] += 1;
    return acc;
  }, [0, 0]);
}

module.exports = { compareTriplets };
