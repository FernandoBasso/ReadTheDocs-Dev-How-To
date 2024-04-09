/*

https://www.codewars.com/kata/delete-occurrences-of-an-element-if-it-occurs-more-than-n-times/train/javascript

*/

const expect = require('expect');

// deleteNth :: arrray-of[number] -> array-of[number]
// Produce array with at most N items of each value.
function deleteNth(arr, max) {
  const hist = {};

  return arr.reduce((acc, n) => {
    hist[n] === undefined ? hist[n] = 1 : hist[n] += 1;

    if (hist[n] <= max) return [...acc, n];

    return acc;
  }, []);

  return [];
}


expect(
  deleteNth([1, 1, 1, 1], 2)
).toEqual([1, 1]);

expect(
  deleteNth([20, 37, 20, 21], 1)
).toEqual([20, 37, 21]);

expect(
  deleteNth([1, 1, 3, 3, 7, 2, 2, 2, 2], 3)
).toEqual([1, 1, 3, 3, 7, 2, 2, 2]);

l('\nAll tests passed!');

/*

This version uses an object to keep track of how many times a number has
showed on the array, and uses that object to decide if a give `n` should
be returned in the final array.

*/
