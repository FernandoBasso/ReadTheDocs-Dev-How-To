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

    const idxToInc = aliceRating > bobRating
      ? 0
      : bobRating > aliceRating
        ? 1
        : undefined;

    if (idxToInc === undefined) return acc;

    acc[idxToInc] += 1;

    return acc;
  }, [0, 0]);
}

module.exports = { compareTriplets };
