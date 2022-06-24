/**
 * The solution from the instructor with some of my own “touch” 🤣 (I
 * did change some syntax, some variable names, etc.) He first sorts the
 * array in order to make it possible to move both pointers. I did not
 * sort the array because that is a somewhat expensive operation
 * nonetheless.
 *
 * This solution doesn't work well with Node because of some
 * Array.prototype.sort() change in V8:
 *
 * • https://github.com/nodejs/node/pull/22754
 *
 * Looks like if we change the `>` in the original sort function by the
 * instructor:
 *
 *   (a, b) => a > b
 *
 * to `-` it works in Node too:
 *
 *   (a, b) => a - b
 */
function areThereDups(...args) {
  ////
  // ‘>’ doesn't work in V8 anymore for this sort thing.
  //
  // args.sort((a, b) => a > b);
  //
  args.sort((a, b) => a - b);

  let start = 0;
  let next = 1;

  while (next < args.length) {
    if (args[start++] === args[next++])
      return true;
  }

  return false;
}

export { areThereDups };
