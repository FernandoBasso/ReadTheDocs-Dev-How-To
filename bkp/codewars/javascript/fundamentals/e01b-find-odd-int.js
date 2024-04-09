//
// https://www.codewars.com/kata/54da5a58ea159efa38000836/train/javascript
//

const l = console.log.bind(console);

/**
 * Find number that appear an odd number of times.
 *
 * ASSUME: There will always be only one integer that appears
 * an odd number of times.
 *
 * @param {array<number>} arr
 * @return {number}
 */
function findOdd(arr) {
  // This ‘find’ and ‘filter’ version only works because of the
  // assumption we made above. If there are more than one number
  // that appears an odd number of times,  then this algorithm
  // would find only the first one.

  // “Find an ‘n’ in the array ‘arr’ which an odd number of times”.
  // ‘filter’ filers all ‘e’s that are equal to ‘n’ and then verifies
  // that the resulting filtered array has an odd (1, 3, 5, etc). ‘length’.
  return arr.find(n => arr.filter(e => e === n).length % 2 !== 0);
}

l(findOdd([20, 1, -1, 2, -2, 3, 3, 5, 5, 1, 2, 4, 20, 4, -1, -2, 5]));
// → 5;

l(findOdd([1, 1, 2, -2, 5, 2, 4, 4, -1, -2, 5]));
// → -1

l(findOdd([20, 1, 1, 2, 2, 3, 3, 5, 5, 4, 20, 4, 5]));
// → 5

l(findOdd([10]));
// → 10

l(findOdd([1, 1, 1, 1, 1, 1, 10, 1, 1, 1, 1]));
// → 10

l(findOdd([5, 4, 3, 2, 1, 5, 4, 3, 2, 10, 10]));
// → 1

/*

Use Array.prototype.filter to find elements that are equal to `n`. If it is
found an odd number of times, `find` returns it.

*/
