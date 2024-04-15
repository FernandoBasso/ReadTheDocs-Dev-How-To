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

  if (aliceRatings[0] > bobRatings[0]) {
    alicePoints += 1;
  }
  else if (aliceRatings[0] < bobRatings[0]) {
    bobPoints += 1;
  }

  if (aliceRatings[1] > bobRatings[1]) {
    alicePoints += 1;
  }
  else if (aliceRatings[1] < bobRatings[1]) {
    bobPoints += 1;
  }

  if (aliceRatings[2] > bobRatings[2]) {
    alicePoints += 1;
  }
  else if (aliceRatings[2] < bobRatings[2]) {
    bobPoints += 1;
  }

  return [alicePoints, bobPoints];
}

module.exports = { compareTriplets };
