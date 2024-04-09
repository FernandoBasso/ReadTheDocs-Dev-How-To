//
// https://www.codewars.com/kata/54da5a58ea159efa38000836/train/javascript
//

const l = console.log.bind(console);

/**
 * Find number that appears an odd number of times.
 *
 * ASSUME: There will always be only one integer that appears
 * an odd number of times.
 *
 * @param {array<number>} arr
 * @return {number}
 */
function findOdd(arr) {
  const obj = arr.reduce((acc,  n) => {
    // If we have not yet started counting the number of appearances
    // of that number...
    if (acc[n] === undefined) {
      // ...then, make that number an index and start it to 1.
      acc[n] = 1;
      return acc;
    }

    // We have seen this number before. Increment its “count” value.
    acc[n] += 1;
    return acc;
  }, {});

  return Object.keys(obj).find(k => obj[k] % 2 !== 0);
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

Create an object where each number is the key, and we increment the
value for that key every time that number appears.

Then we find which key in the object has an odd number as its value.

*/
