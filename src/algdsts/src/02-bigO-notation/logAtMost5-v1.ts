/**
 * Logs an int from 1 to n or 5, whichever is smaller.
 *
 * ASSUME: `n` is an int > 0.
 *
 * **TIME COMPLEXITY**: `O(1)`. Constant time because we never run this
 * more than a few times. As `n` grows, we never log more than 5 times.
 *
 * **SPACE COMPLEXITY**: `O(1)`. Constant because we just increment a
 * single numeric variable.
 *
 * @param n
 */
function logAtMost5(n: number): void {
  for (let i = 0; i < Math.min(n, 5); ++i)
    console.log(i);
}

export { logAtMost5 };
