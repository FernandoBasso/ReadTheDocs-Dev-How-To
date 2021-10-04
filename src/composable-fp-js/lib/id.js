/**
 * The identity function.
 *
 * Simply returns the input untouched.
 *
 * @param {any} x
 * @return {any} x
 *
 * @example
 * id(1);
 * // → 1
 *
 * id({ jedi: 'Yoda' });
 * // → { jedi: 'Yoda' }
 */
function id(x) {
  return x;
}

export { id }
