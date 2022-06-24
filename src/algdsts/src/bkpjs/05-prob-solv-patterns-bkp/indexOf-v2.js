/**
 * Finds the index of elem in arr. Returns -1 if not found.
 *
 * **TIME COMPLEXITY**: `O(log(n))`. Binary search using the “divide and
 * conquer” pattern.
 *
 * **SPACE COMPLEXITY**: `O(1)`. We just store and update a few
 * primitive numeric variables.
 *
 * @param {number} val
 * @param {Array<number>} arr A sorted array of integers.
 */
function indexOf(val, arr) {
  if (arr.length === 0) return -1;

  let min = 0;
  let max = arr.length;
  let mid;

  while (min <= max) {
    mid = Math.floor((min + max) / 2);

    if (arr[mid] < val)
      min = mid + 1;
    else if (arr[mid] > val)
      max = mid - 1;
    else
      return mid;
  }

  return -1;
}

export { indexOf };
