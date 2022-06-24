/**
 * Checks if there are any duplicate values among the arguments.
 *
 * Solution using the “multiple pointers” pattern.
 *
 * **TIME COMPLEXITY**: `O(n)`. We potentially end up iterating over the
 * whole input.
 *
 * **SPACE COMPLEXITY**: `O(1)`. Just use a few primitive numeric
 * variables to control the flow.
 *
 * @param {...(number|string)}
 * @return {boolean}
 *
 * <1> Since the input is not sorted, I do not know how to use two
 * pointers and move them in such a way that less iterations are
 * required.
 *
 */
function areThereDups(...args) {
  if (args.length === 0) return false;

  let l = 0, r = args.length - 1;

  while (l < r) { // <1>
    if (args[l++] === args[r]) {
      return true;
    }
  }

  return false;
}

export { areThereDups };
