const l = console.log.bind(console);

/**
 * A manually curried function to check types.
 *
 * @param {ConstructorFunction} someConstructor
 * @returns {function(any): boolean}
 */
function isA(someConstructor) {
  return function isType (thing) {
    if (!someConstructor) return undefined;
    return thing.constructor === someConstructor;
  }
}

var isFunction = isA(Function);
var isRegExp = isA(RegExp);

l(isFunction(() => undefined));
l(isFunction({}));
l(isRegExp(/t/));
l(isRegExp(new RegExp('')));

/* vim: set tw=68 ft=text ai: */

