/**
 * Sums an array of numbers.
 *
 * This solution uses a reducing function in a more FPish way. The
 * reducing function is defined in place through an arrow function.
 *
 * **TIME COMPLEXITY**: O(n). We iterate once for each element of the
 * input array of numbers.
 *
 * **SPACE COMPLEXITY**: O(1). We simply add to the `total` variable.
 *
 * @param xs The array of numbers to sum.
 * @returns The sum.
 */
function sum(xs: number[]): number {
  return xs.reduce((acc: number, n: number): number => {
    return acc + n;
  }, 0);
}

export { sum };
