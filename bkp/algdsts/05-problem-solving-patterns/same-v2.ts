/**
 * Returns `true` if each element in `xs` has a matching squared element
 * in `ys`; `false` otherwise. Order doesn't matter. Frequency matters.
 *
 * @param xs
 * @param ys
 * @returns
 */
function same(xs: number[], ys: number[]): boolean {
  if (xs.length !== ys.length) return false;

  const freqXs: Record<string, number> = {};
  const freqYs: Record<string, number> = {};

  for (const x of xs) {
    freqXs[x] = (freqXs[x] || 0) + 1;
  }

  for (const y of ys) {
    freqYs[y] = (freqYs[y] || 0) + 1;
  }

  for (const k in freqXs) {
    // Check if x² exists in ys. Compare the keys.
    if (!(freqXs[k] ** 2 in freqYs)) return false;

    // Check if number of occurrences match. Compare the values.
    if (freqYs[freqXs[k] ** 2] !== freqXs[k]) return false;
  }

  return true;
}

export { same };
