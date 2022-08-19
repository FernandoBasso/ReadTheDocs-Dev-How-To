
/**
 * Returns the first character of the input string.
 *
 * @param {string} str
 * @returns {string}
 *
 * @example
 * head('abc');
 * // → 'a'
 */
function head(str) {
  return str.slice(0, 1);
}

/**
 * Returns the string without the first character.
 *
 * @param {string} str
 * @return {string}
 *
 * @example
 * tail('abc');
 * // → 'bc'
 */
function tail(str) {
  return str.slice(1);
}

/**
 * Capitalizes the first char of input string.
 *
 * @param {string} str
 * @return {string}
 *
 * @example
 * capitalize('abc');
 * // → 'Abc'
 */
function capitalize(str) {
  return str.toUpperCase();
}

/**
 * Capitalizes all words of the string.
 *
 * @param {string} sentence
 * @returns {string}
 */
function capitalizeAll(sentence) {
  const words = sentence.split(' ');

  // TODO: This code is not using these
  // helper functions correctly.
  // FIXME!

  return words.map(word => {
    const first = head(word);
    const rest = tail(word);

    return capitalize(first) + rest;
  }).join(' ');
}

module.exports = { capitalizeAll };

//
// • head - the first element of a list
// • tail - the list without the first element
// • take - the first n elements of the list
// • drop (or skip) - the list without the first n elements
//
