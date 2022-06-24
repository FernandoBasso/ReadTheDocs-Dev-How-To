
function keyIn(key, obj) {
  return key in obj;
}

/**
 * Returns true if each element in `xs` has a matching squared element in
 * `ys`. Order doesn't matter. Frequency matters.
 *
 * **TIME COMPLEXITY**: `O(n)`. Three loops is still linear time. Much
 * better than two nested loops.
 *
 * **SPACE COMPLEXITY**: `O(n)`. We create two other objects with the
 * same number of keys as `xs` and `ys`. So, the space taken inside the
 * function is directly proportional to the input arrays size.
 *
 * @param {Array<number>} xs
 * @param {Array<number>} ys
 * @return {boolean}
 */
function same(xs, ys) {
  if (xs.length !== ys.length) return false;

  const freqXs = {};
  const freqYs = {};

  for (let x of xs) {
    freqXs[x] = (freqXs[x] || 0) + 1;
  }

  for (let y of ys) {
    freqYs[y] = (freqYs[y] || 0) + 1;
  }

  for (let k in freqXs) {
    // Check if x² exists in ys. Compare the keys.
    if (!(k ** 2 in freqYs)) return false;

    // Check if number of occurrences match. Compare the values.
    if (freqYs[k ** 2] !== freqXs[k]) return false;
  }

  return true;
}

export { same };

//
// Three loops is much better than a loop inside a loop.
// • Two nested loops: O(n ^ 2) -- quadratic.
// • Three loops one after the other: O(n).
//
// Three loops is O(3n) but is simplified by O(n). It is still linear
// time.
//
