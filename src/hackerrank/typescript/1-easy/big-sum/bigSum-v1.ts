/**
 * Sums an array of numbers.
 *
 * This solution uses a for loop with a procedural style.
 *
 * @param xs The array of numbers to sum.
 * @returns The sum.
 */
function sum(xs: number[]): number {
  let total: number = 0;

  for (let i: number = 0; i < xs.length; ++i)
    total += xs[i];

  return total;
}

export { sum };
