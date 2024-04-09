/**
 * Adds positive integers from 0 up to and including `n`.
 *
 * **TIME COMPLEXITY**: `O(1)`. We simply perform three arithmetic
 * operations and nothing else.
 *
 * **SPACE COMPLEXITY**: `O(1)`. We do not create and store any values
 * or data structures inside the function.
 *
 * @param {number} n
 * @return {number}
 */
function addUpTo(n) {
  return n * (n + 1) / 2;
}

export { addUpTo };

