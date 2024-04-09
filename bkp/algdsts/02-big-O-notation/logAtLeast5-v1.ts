/**
 * Logs an int from 1 to 5 or `n` times, whichever is larger.
 *
 * **TIME COMPLEXITY**: `O(n)`. It runs the loop from 5 times to
 * possibly infinite times. There are are as many loops as is the size
 * of `n`.
 *
 * **SPACE COMPLEXITY**: `O(1)`. No extra space is used besides setting
 * and incrementing a single numeric variable.
 *
 * @param n The minimum number of times to log.
 */
function logAtLeast5(n: number): void {
  for (let i = 0; i < Math.max(n, 5); ++i)
    console.log(n);
}

export { logAtLeast5 };
