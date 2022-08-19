/**
 * Capitalizes all words of the string.
 *
 * @param {string} sentence
 * @returns {string}
 *
 * @example
 * capitalizeAll('');
 * // -> ''
 *
 * @example
 * capitalizeAll('a b c');
 * // -> 'A B C'
 *
 * @example
 * capitalizeAll('hello world');
 * // -> 'Hello World'
 */
function capitalizeAll(sentence) {
  const words = sentence.split(' ');

  return words.map(word => {
    const first = word.slice(0, 1);
    const rest = word.slice(1);

    return first.toUpperCase() + rest;
  }).join(' ');
}

module.exports = { capitalizeAll };

//
// > 'hello'.slice(0, 1)
// 'h'
// > 'hello'.slice(1)
// 'ello'
//
// > ["Hello", "World"].join(' ')
// 'Hello World'
// > ["Hello", "World"].join('-')
// 'Hello-World'
//
// > w
// 'abc'
//
// > w.slice(0, 1)
// 'a'
//
// > w.slice(1)
// 'bc'
//
// > w.slice(0, 1) + w.slice(1)
// 'abc'
//
// > w.slice(0, 1).toUpperCase() + w.slice(1)
// 'Abc'
//
// Array.prototype.slice does NOT mutate the array.
// Array.prototype.splice DOES MUTATE the array.
//
