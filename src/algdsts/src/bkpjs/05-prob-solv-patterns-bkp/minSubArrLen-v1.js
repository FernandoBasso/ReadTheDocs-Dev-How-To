//
// Sliding Window - minSubArrLen
// ===============================
//
// Write a function called minSubArrLen which accepts two parameters -
// an array of positive integers and a positive integer.
//
// This function should return the minimal length of a contiguous
// subarray of which the sum is greater than or equal to the integer
// passed to the function. If there isn't one, return 0 instead.
//
// Examples:
// =========
//
// minSubArrayLen([2, 3, 1, 2, 4, 3], 7);
// 2 because [4, 3] is the smallest subarray
//
// minSubArrayLen([2, 1, 6, 5, 4], 9);
// 2 because [5, 4] is the smallest subarray
//
// minSubArrayLen([3, 1, 7, 11, 2, 9, 8, 21, 62, 33, 19], 52);
// 1 because [62] is greater than 52
//
// minSubArrayLen([1, 4, 16, 22, 5, 7, 8, 9, 10], 39);
// 3
//
// minSubArrayLen([1, 4, 16, 22, 5, 7, 8, 9, 10], 55);
// 5
//
// minSubArrayLen([4, 3, 3, 8, 1, 2, 3], 11);
// 2
//
// minSubArrayLen([1, 4, 16, 22, 5, 7, 8, 9, 10], 95);
// 0
//
// • Time Complexity - O(n)
// • Space Complexity - O(1)
//

/**
 * Finds the minimum subarray length l such that when summed is >= n
 *
 * **TIME COMPLEXITY**: `O(n²)` (TODO: is it really O(n²)‽). We iterate
 * over the array once, but also reduce to sum slices of the input every
 * time.
 *
 * **SPACE COMPLEXITY**: `O(1)`. We just store some numbers.
 *
 * @param {Array<number>} nums
 * @param {number} n
 * @return {number}
 */
function minSubArrLen(nums, n) {
  let l = 0;
  let r = 1;
  let subArrLen = 1;
  let sum = nums[0];

  while (l < nums.length) {
    if (sum >= n) return subArrLen;

    if (r === nums.length) {
      ++subArrLen;
      l = 0;
      r = subArrLen;

      sum = nums.slice(0, subArrLen).reduce((acc, x) => acc + x, 0);
      if (sum >= n) return subArrLen;

      if (subArrLen >= nums.length) return 0;

      continue;
    }

    ++l;

    sum = sum - nums[l - 1] + nums[r];

    ++r;
  }

  return 0;
}

export { minSubArrLen };
