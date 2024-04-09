//
// Given a sorted array of integers, write a function called indexOf,
// that accepts a value and returns the index where the value passed to
// the function is located. If the value is not found, return -1
//

/**
 * Finds the index of elem in arr. Returns -1 if not found.
 *
 * **TIME COMPLEXITY**: `O(n)`. We (potentially) iterate over every
 * element in the input array.
 *
 * **SPACE COMPLEXITY**: `O(1)`. We do not use data structures that grow
 * as the input grows. Just a numeric variable that is incremented in
 * the loop.
 *
 * @param {number} elem
 * @param {Array<number>} arr A sorted array of integers.
 */
function indexOf(elem, arr) {
  for (let idx = 0; idx < arr.length; ++idx) {
    if (elem === arr[idx]) return idx;
  }

  return -1;
}

export { indexOf };
