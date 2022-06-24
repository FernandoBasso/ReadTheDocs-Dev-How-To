/**
 * Computes the product of the elements of `nums`.
 *
 * **TIME COMPLEXITY**: `O(n)` since we recurse consuming all the
 * elements. That means the number of iterations is increases as the
 * input increases.
 *
 * **SPACE COMPLEXITY**: `O(1)` since do not create an array or other
 * complex data structure that grows as input grows.
 *
 * @param {Array<number>} nums
 * @return {number}
 */
function arrProduct(nums) {
  if (nums.length === 0) return 1;
  return nums[0] * arrProduct(nums.slice(1));
}

export { arrProduct };
