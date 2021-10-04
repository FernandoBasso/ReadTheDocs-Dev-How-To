/**
 * Log the arguments to the console.
 *
 * I mostly use this when I am playing with some idea and I run the npm
 * scripts (see package.json) like this:
 *
 *   $ npm run file vidN/foo.js
 *
 * or
 *
 *   $ npm run watch vidN/foo.js
 *
 * Especially with watch, I then have delimiting “----”-like line to
 * help visualize where each log starts.
 *
 * @param {...any} args Zero or more values to log.
 * @return {Array<args>}.
 *
 * @example
 * log('hey');
 * // → --------------------------------
 * // → hey
 *
 * @example
 * log('h4ck3r, [1, 2]);
 * // → --------------------------------
 * // → h4ck3r
 * // → [ 1, 2 ]
 */
function log(...args) {
  console.log('-'.repeat(32));

  args.forEach(function logArg(arg) {
    console.log(arg);
  });

  // Print an empty line.
  console.log();

  return args;
}

export { log };
