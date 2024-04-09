
/**
 * Computes a tuple with alice and bob's points.
 *
 * @param {number[]} aliceRatings
 * @param {number[]} bobRatings
 * @returns {number[]}
 */
function compareTriplets(aliceRatings, bobRatings) {
  let alicePoints = 0;
  let bobPoints = 0;

  for (let i = 0; i <= 2; i++) {
    if (aliceRatings[i] > bobRatings[i]) alicePoints += 1;
    else if (aliceRatings[i] < bobRatings[i]) bobPoints += 1;
  }

  return [alicePoints, bobPoints];
}

module.exports = { compareTriplets };
