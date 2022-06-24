/**
 * Returns only elements of `xs` whose index is even.
 *
 * **TIME COMPLEXITY**: `O(n)`. Linear time because we have to loop for
 * as many times as the length of the array.
 *
 * **SPACE COMPLEXITY**: `O(n)`. We have to create an output array of
 * half of the size of the input array, but it grows according the
 * length of the input array.
 *
 * @param {Array<unknown>} xs
 * @param {Array<unknown>}
 */
function onlyAtEvenIndex(xs) {
  const atEvenIndex = [];

  for (let i = 0; i < xs.length; ++i) {
    if (i % 2 === 0) {
      atEvenIndex.push(xs[i]);
    }
  }

  return atEvenIndex;
}

export { onlyAtEvenIndex };
