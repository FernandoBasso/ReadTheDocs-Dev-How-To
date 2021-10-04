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

export { isNil }
