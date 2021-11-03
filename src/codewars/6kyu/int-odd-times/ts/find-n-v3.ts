
/**
 * Returns the element that appears an odd number of times in `nums`.
 */
function findN(nums: number[]): number {
  return nums.reduce((acc, n) => acc ^ n);
}

export { findN };
