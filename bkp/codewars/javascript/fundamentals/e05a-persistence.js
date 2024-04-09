//
// https://www.codewars.com/kata/persistent-bugger/train/javascript
//

const l = console.log.bind(console);

/**
 * Multiply two numbers.
 * 
 * @param {number} x
 * @param {number} y
 * @return {number}
 */
const multiply = (x, y) => x * y;

/**
 * Parse a number into an array of its digits.
 *
 * https://en.wikipedia.org/wiki/Persistence_of_a_number
 * 
 * @param {number} n
 * @return {array<number>}
 */
const toDigits = (n) => String(n).split('').map(Number);

/**
 * Find the _persistence_ of a number.
 * 
 * @param {number} num
 * @param {number} [count = 0]
 */
function persistence(num, count = 0) {

  ////
  // The base case.
  if (num < 10) return count;

  ////
  // Recurse, incrementing the counter. \o/
  // We must eventually reach the base case and return `count`.
  return persistence(toDigits(num).reduce(multiply), count + 1);
}

l(persistence(4));
// → 0
// because 4 is already a one-digit number.

l(persistence(39));
// → 3
// because 3 * 9 = 27, 2 * 7 = 14, 1 * 4 = 4  and 4 has only one digit

l(persistence(999));
// → 4
// because 9 * 9 * 9 = 729, 7 * 2 * 9 = 126,  1 * 2 * 6 = 12,
// and finally 1 *, 2 = 2, one digit.

/*

NOTE: Technically, we don't need to `map(Number)` because the multiplication operator
`*` convertes string numbers to numbers.

https://en.wikipedia.org/wiki/Persistence_of_a_number

*/
