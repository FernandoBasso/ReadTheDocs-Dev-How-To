
/**
 * Returns the first character of the input string.
 *
 * @param {string} str
 * @returns {string}
 */
function head(str) {
  return str.slice(0, 1);
}

/**
 * Returns the string without the first character.
 *
 * @param {string} str
 * @return {string}
 */
function tail(str) {
  return str.slice(1);
}

/**
 * Capitalize first char of input string.
 *
 * @param {string} str
 * @return {string}
 */
function capitalize(str) {
  return str.toLocaleUpperCase();
}

/**
 * Capitalizes all words of the string.
 *
 * @param {string} sentence
 * @returns {string}
 */
function capitalizeAll(sentence) {
  const words = sentence.split(' ');

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
