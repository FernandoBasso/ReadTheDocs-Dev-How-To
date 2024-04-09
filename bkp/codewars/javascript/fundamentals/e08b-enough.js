/*

https://www.codewars.com/kata/delete-occurrences-of-an-element-if-it-occurs-more-than-n-times/train/javascript

*/

const expect = require('expect');
const l = console.log.bind(console);

// deleteNth :: List-of-number -> list-of-number
// Produce array with at most N items of each value.
function deleteNth(arr, max) {
  return arr.reduce((acc, x) => {
    if (acc.filter(y => y === x).length >= max) return acc;
    return [...acc, x];
  }, []);
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

expect(
  deleteNth([3, 2, 3, 3, 3, 2, 3, 2, 3, 2, 3], 2)
).toEqual([3, 2, 3, 2])

l('\nAll tests passed!\n');

/*

Here we use the accumulator itself to decide if an element has already
exceeded the maximum number of times it is allowed to appear.

*/
