/**
 * Sum an all the elements in `xs`.
 *
 * This solution uses a reducing function, which makes it a bit FP-ish.
 *
 * **TIME COMPLEXITY**: O(n). The number of times we add is proportional
 * to the length of the input.
 *
 * **SPACE COMPLEXITY**: O(1). `acc` (a number) is simply added to,
 * which does not cause the algorithm to take any further space than the
 * space require to store a number.
 *
 * @param xs The array of numbers to sum.
 * @return The total.
 */
function sum(xs: number[]): number {
  return xs.reduce((acc: number, x: number): number => {
    return acc + x;
  }, 0);
}

export { sum };
