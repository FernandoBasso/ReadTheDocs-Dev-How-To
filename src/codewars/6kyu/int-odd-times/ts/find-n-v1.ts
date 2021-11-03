
/**
 * Returns the element that appears an odd number of times in `nums`.
 */
function findN(nums: number[]): undefined | number {
  return nums.find(function hasOddOccurrences(x) {
    return nums.filter(y => y === x).length % 2 !== 0;
  });
}

export { findN };
