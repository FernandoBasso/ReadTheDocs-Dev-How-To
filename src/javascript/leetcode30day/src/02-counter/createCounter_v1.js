/**
 * @param {number} n
 * @returns {() => number}
 */
function createCounter(n) {
  var x = n;

  return function counter() {
    return x++;
  };
}

export { createCounter };
