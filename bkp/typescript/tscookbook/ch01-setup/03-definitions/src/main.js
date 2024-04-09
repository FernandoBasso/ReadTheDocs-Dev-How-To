// \@ts-check

/** @typedef { import('./types/defs').Person } Person */
/** @typedef { import('./types/defs').Student } Student */

const log = console.log.bind(console);

/**
 * @param {Student} student
 * @returns {void}
 */
function print({ foo }) {
  log(foo);
}

