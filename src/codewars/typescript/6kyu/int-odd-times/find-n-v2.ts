
/**
 * Counts the number of occurrences of `x` in `xs`.
 */
function countX(x: number, xs: number[]): number {
  return xs.filter(val => val === x).length;
}

/**
 * Checks whether `x` appears an odd number of times in `xs`.
 */
function hasOddOccurrences(xs: number[]): (x: number) => boolean {
  return function count(x: number): boolean {
    return countX(x, xs) % 2 !== 0;
  };
}

/**
 * Returns the element that appears an odd number of times in `nums`.
 */
function findN(nums: number[]): undefined | number {
  return nums.find(hasOddOccurrences(nums));
}

export { findN };
