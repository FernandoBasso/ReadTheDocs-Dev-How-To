
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

    if (aliceRating > bobRating) acc[0] += 1;
    else if (aliceRating < bobRating) acc[1] += 1;

    return acc;
  }, [0, 0]);
}

module.exports = { compareTriplets };
