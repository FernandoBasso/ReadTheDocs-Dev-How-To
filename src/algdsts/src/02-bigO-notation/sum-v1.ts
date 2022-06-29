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
 * This solution uses a very imperative approach with a for loop.
 *
 * @param xs The array of numbers to sum.
 * @return The total.
 */
function sum(xs: number[]): number {
  let total = 0;

  for (let i = 0; i < xs.length; ++i)
    total += xs[i];

  return total;
}

export { sum };
