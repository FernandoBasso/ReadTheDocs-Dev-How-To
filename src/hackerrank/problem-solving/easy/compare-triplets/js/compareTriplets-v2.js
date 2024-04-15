const RATINGS_LEN = 3;

/**
 * Computes a tuple with alice and bob's points.
 *
 * - T.C: O(n).
 * - S.C: O(1).
 *
 * @param {[number, number, number]} aliceRatings
 * @param {[number, number, number]} bobRatings
 * @returns {[number, number]}
 */
function compareTriplets(aliceRatings, bobRatings) {
  let alicePoints = 0;
  let bobPoints = 0;

  for (let i = 0; i <= RATINGS_LEN; i++) {
    if (aliceRatings[i] > bobRatings[i])
      alicePoints += 1;
    else if (aliceRatings[i] < bobRatings[i])
      bobPoints += 1;
  }

  return [alicePoints, bobPoints];
}

module.exports = { compareTriplets };
