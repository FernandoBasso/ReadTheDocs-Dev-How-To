/**
 * Logs an int from 1 to 5 or `n` times, whichever is larger.
 *
 * **TIME COMPLEXITY**: `O(n)`. It runs the loop from 5 times to
 * possibly infinite times. There are are many loops as is the size of
 * `n`.
 *
 * **SPACE COMPLEXITY**: `O(1)`. No extra space is used besides setting
 * and incrementing a single numeric variable.
 *
 * @param {number} n
 */
function logAtLeast5(n) {
  for (let i = 1; i <= Math.max(5, n); ++i) {
    console.log(i);
  }
}

export { logAtLeast5 };
