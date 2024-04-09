//
// Write a function called maxSubArrSum which accepts an array of
// integers and a number called n. The function should calculate the
// maximum sum of n consecutive elements in the array.
//

/**
 * Finds the maximum sum of `n` consecutive elements in `arr`.
 *
 * Returns null with empty array input, or the max sum found.
 *
 * ASSUME: The input array does not contain empty slots.
 *
 * **TIME COMPLEXITY**: `O(n)`. We iterate only once.
 *
 * **SPACE COMPLEXITY**: We just store and update a few primitive values
 * inside the function.
 *
 * @param {Array<number>} nums An array of integers
 * @param {number} n How many consecutive elements to sum. Must NOT be
 *  more than the length of the input array (n <= nums.length).
 * @return {NaN|null|number}
 */
function maxSubArrSum(nums, n) {
  if (nums.length === 0 || nums.length < n) return null;

  let maxSoFar = -Infinity; // <1>
  let i, slidingWindowSum;

  maxSoFar = nums.slice(0, n).reduce((acc, num) => acc + num, 0);
  slidingWindowSum = maxSoFar;

  for (i = n; i < nums.length; ++i) {
    slidingWindowSum = slidingWindowSum - nums[i - n] + nums[i];
    maxSoFar = Math.max(slidingWindowSum, maxSoFar);
  }

  return maxSoFar;
}

//
// === <1> ===
// ===========
// -Infinity because we need to take the fact that the sum could be
// negative into account. Starting with 0 would be a bad judgment here.
//

export { maxSubArrSum };
