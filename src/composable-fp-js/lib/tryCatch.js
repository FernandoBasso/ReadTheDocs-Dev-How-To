import { Left, Right } from './Either.js';

/**
 * Turns a try/catch into an `Either`, composable container.
 *
 * @param {Function} f
 * @return {Either}
 */
const tryCatch = f => {
  try {
    return Right(f());
  } catch (err) {
    return Left(err);
  }
};

export { tryCatch }
