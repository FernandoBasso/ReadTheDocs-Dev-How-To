import { isNil } from './isNil.js';
import { Left, Right } from './Either.js';

/**
 * Wraps the value into an `Either` type.
 *
 * @param {any} value
 * @return {Either}
 */
function fromNullable(value) {
  return isNil(value) ? Left(value) : Right(value);
}

export { fromNullable }
