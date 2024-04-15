/**
 * Finds the indices of two distinct indices in `flavors` that when
 * added together are equal to `money`.
 *
 * NOTE: The challenge requires the output indices to start at 1 😅.
 *
 * ASSUME:
 * • There is always a single, correct solution.
 * • The input array is NOT sorted.
 * • The input array could contain duplicate values.
 *
 * @sig Int -> [Int] -> [Int, Int]
 * @param {number} money
 * @param {Array<number>} flavorPrices
 * @returns {[number, number]}
 */
function iceCreamParlor(money, flavorPrices) {
  var len = flavorPrices.length,
      i,
      j;

  for (i = 0; i < len; ++i) {
    for (j = i + 1; j < len; ++j) {
      if (flavorPrices[i] + flavorPrices[j] === money) {
        return [i + 1, j + 1];
      }
    }
  }
}

export { iceCreamParlor };
