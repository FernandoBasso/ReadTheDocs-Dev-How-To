
/**
 * Replaces pattern with replacements in the input string.
 *
 * Returns unefined if parameters are invalid.
 *
 * @param {string|regexp} pattern The pattern to match
 * @param {string} replacement What to substitute pattern with
 * @param {string} str The input string to be operated on
 * @return {undefined|string}
 */
export function replace(pattern, replacement, str) {
  if (str === undefined || str === null) return undefined;
  if (str.constructor !== String) return undefined;

  return str.replace(pattern, replacement);
};

