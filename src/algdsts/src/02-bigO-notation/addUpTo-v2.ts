/**
 * Adds positive integers from 0 up to and including `n`.
 *
 * **TIME COMPLEXITY**: `O(1)`. Uses clever mathy thing to avoid use of
 * loops altogether.
 *
 * **SPACE COMPLEXITY**: `O(1)`. We do not create and store any values
 * or data structures inside the function.
 *
 * @param {number} n
 * @return {number}
 */
function addUpTo(n: number): number {
  return n * (n + 1) / 2;
}

export { addUpTo };
