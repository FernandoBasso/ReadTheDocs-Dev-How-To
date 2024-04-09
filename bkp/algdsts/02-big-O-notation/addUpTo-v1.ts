/**
 * Adds positive integers from 0 up to and including `n`.
 *
 * **TIME COMPLEXITY**: `O(n)`. We run a loop `n` times.
 *
 * **SPACE COMPLEXITY**: `O(1)`. We simply increment one single value
 * inside the function.
 *
 * @param n
 * @return
 */
function addUpTo(n: number): number {
  let total: number = 0;

  for (let i = 0; i <= n; ++i) {
    total += i;
  }

  return total;
}

export { addUpTo };

