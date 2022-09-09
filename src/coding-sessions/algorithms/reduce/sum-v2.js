/**
 * Sums the numbers of the array of numbers.
 *
 * @param {number[]} xs
 * @returns {number}
 * @example
 * sum([1, 2, 3]);
 * // → 6
 */
function sum(xs) {
  const total = xs.reduce(function performSum(acc, num) {
    return acc + num;
  }, 0);

  return total;
}

module.exports = { sum };

/*

f(acc, currVal)

const total = [1, 2, 3].reduce(function (acc, num) {
  acc = 0
  num = 1
  acc = acc + 1
  return acc; // 1

  acc = 1
  num = 2
  acc = acc + num // 3
  return acc

  acc = 3
  num = 3
  acc = acc + num // 6
  return acc
}, 0);
   ^
    \
     \
  initial value

*/
