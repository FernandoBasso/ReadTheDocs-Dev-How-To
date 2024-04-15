// @ts-check

/**
 * Update points for the winners.
 *
 * @param {[number, number]} points
 * @param {number} aliceRating
 * @param {number} bobRating
 * @returns {[number, number]}
 */
function updatePoints(points, aliceRating, bobRating) {
  const [a, b] = points;

  if (aliceRating > bobRating) return [a + 1, b];
  else if (bobRating > aliceRating) return [a, b + 1];

  return [a, b];
}

/**
 * Computes a tuple with alice and bob's points.
 *
 * @param {number[]} aliceRatings
 * @param {number[]} bobRatings
 * @returns {number[]}
 */
function compareTriplets(aliceRatings, bobRatings) {
  /**
   * @type {[number, number]}
   */
  const initialAcc = [0, 0];

  return aliceRatings.reduce((acc, aliceRating, idx) => {
    const bobRating = bobRatings[idx];
    return updatePoints(acc, aliceRating, bobRating);
  }, initialAcc);
}

module.exports = { compareTriplets };
