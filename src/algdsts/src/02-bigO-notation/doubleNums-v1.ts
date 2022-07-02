/**
 * Doubles the input `xs`.
 *
 * The input array of numbers is **not** modified in place. Instead, a
 * new array with the doubled numbers is returned.
 *
 * This solution uses a very imperative approach with a for loop.
 *
 * **TIME COMPLEXITY**: O(n). As the size of `xs` grow, so does the
 * number of operations grows.
 *
 * **SPACE COMPLEXITY**: O(n). As the size of `xs` grow, so does the
 * space fo the copy of the array with doubled values grow.
 *
 * @param xs The input array of numbers.
 * @returns The doubled array.
 */
function doubleNums(xs: number[]): number[] {
  const doubled: number[] = [];

  for (let i = 0; i < xs.length; ++i)
    doubled[i] = xs[i] * 2;

  return doubled;
}

export { doubleNums };
