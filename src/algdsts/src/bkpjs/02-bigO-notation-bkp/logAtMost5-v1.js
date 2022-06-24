/**
 * Logs an int from 1 to n or 5, whichever is larger.
 *
 * **TIME COMPLEXITY**: `O(1)`. Constant time because we never run this
 * more than a few times.
 *
 * **SPACE COMPLEXITY**: `O(1)`. Constant because we just increment a
 * single numeric variable.
 *
 * @param {number} n
 */
function logAtMost5(n) {
  for (let i = 1; i <= Math.min(n, 5); ++i) {
    console.log(i);
  }
}

export { logAtMost5 };
