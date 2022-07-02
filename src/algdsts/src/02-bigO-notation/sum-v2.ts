/**
 * Sum an all the elements in `xs`.
 *
 * We decided to make an empty array sum result in zero because
 * that is what Haskell's Prelude `sum` does. It ought to be a
 * reasonable default for sum operations on lists and arrays.
 *
 *   $ stack exec -- ghci
 *   GHCi, version 9.0.2: https://www.haskell.org/ghc/  :? for help
 *   ghci> sum []
 *   0
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
