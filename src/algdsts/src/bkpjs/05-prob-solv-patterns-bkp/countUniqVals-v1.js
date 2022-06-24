//
// Implement a function called countUniqVals, which accepts a sorted
// array, and counts the unique values in the array. There can be
// negative numbers in the array, but it will always be sorted.
//

/**
 * Count unique values in the input array of numbers.
 *
 * ASSUME: The input is sorted and does not contain empty slots.
 *
 * **TIME COMPLEXITY**: `O(n)`. We have to iterate once through the
 * entire input.
 *
 * **SPACE COMPLEXITY**: `O(1)`. We do not create a new array that grows
 * as the input grows. Just a few control variables are necessary.
 *
 * @param {Array<number>} nums
 * @return {NaN|number}
 */
function countUniqVals(nums) {
  if (nums.length === 0) return 0;

  // <1>
  let i = 0, count = 1;

  for (; i < nums.length - 1; ++i) {
    if (nums[i] !== nums[i + 1]) ++count; // <2>
  }

  return count;
}

export { countUniqVals };

//
// === <1> ===
// ===========
// This solutions uses only one “pointer” i. The other pointer is
// computed from the pointer i. Since the array of number is sorted, we
// know fore sure that if [i] !== [i + 1] then we have a new different
// value, and that equal values are always siblings (given the fact that
// the input is sorted).
//
// === <2> ===
// ===========
// Also, this solution compares [i] with undefined if the input contains
// only a single number because then [i + 1] will be undefined.
//
// We could return 1 if nums.length is 1, so we know we only enter the
// loop in case we are sure to have at least two values in the array and
// that comparison with undefined would not happen.
//
