const l = console.log.bind(console);

function id(x) {
  return x;
}

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
 * @param {any} value
 * @return {boolean}
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
