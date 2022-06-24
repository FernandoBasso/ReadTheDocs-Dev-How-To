//
// Frequency Counter / Multiple Pointers - areThereDups
//
// Implement a function called areThereDups which accepts a variable
// number of arguments, and checks whether there are any duplicates
// among the arguments passed in. You can solve this using the frequency
// counter pattern OR the multiple pointers pattern.
//
// Examples:
// areThereDuplicates(1, 2, 3); // false
// areThereDuplicates(1, 2, 2); // true
// areThereDuplicates('a', 'b', 'c', 'a'); // true
//
// Restrictions:
// Time - O(n)
// Space - O(n)
//
// Bonus:
// Time - O(n log n)
// Space - O(1)
//

/**
 * Checks if there are any duplicate values among the arguments.
 *
 * Solution using the “frequency counter” pattern.
 *
 * **TIME COMPLEXITY**: `O(n)`. We potentially iterate over the entire
 * input.
 *
 * **SPACE COMPLEXITY: `O(n)`. We potentially create a frequency object
 * as large as the input.
 *
 * @param {...(number|string|nul|undefined)}
 * @return {boolean}
 */
function areThereDups(...args) {
  if (args.length === 0) return false;

  const freqs = {};

  for (let val of args) {
    if (freqs[val]) return true;
    freqs[val] = 1;
  }

  return false;
}

export { areThereDups };
