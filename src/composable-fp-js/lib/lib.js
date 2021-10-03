const l = console.log.bind(console);

function id(x) {
  return x;
}

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
  l('-'.repeat(32));

  args.forEach(function logArg(arg) {
    l(arg);
  });

  // Print an empty line.
  l();

  return args;
}

/**
 * Checks whether `value` is `undefined` or `null`.
 *
 * Returns `true` if, and only if, `value` is `undefined` or `null`;
 * return `false` for any other value, including `false` empty string or
 * array, etc.
 *
 * @sig * -> Boolean
 *
 * @param {any} value
 * @return {boolean}
 *
 * @example
 * isNil(undefined);
 * // → true
 *
 * isNil(null);
 * // → true
 *
 * isNil(false);
 * // → false
 *
 * isNil(0);
 * // → false
 *
 * isNil('');
 * // → false
 *
 * isNil(NaN);
 * // → false
 *
 * isNil([]);
 * // → false
 */
function isNil(value) {
  return value === undefined || value === null;
}

export {
  l,
  id,
  log,
  isNil,
};

log('hey');
log('h4ck3r', [1, 2]);
