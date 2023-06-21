var log = console.log.bind(console);

/**
 * Creates logger functions.
 *
 * @example
 * var info = makeLogger('INFO');
 * info('System running');
 * //=> INFO: System running
 *
 * @sig String -> String -> String
 */
function makeLogger(type) {
  return function logger(msg) {
    console.log('testing');
    log(`${type}: ${msg}`);
  }
}

var info = makeLogger('INFO');

info('System Running.');
