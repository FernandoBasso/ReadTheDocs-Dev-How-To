/**
 * @returns {Function}
 */
function createHelloWorld() {
  return function hello() {
    return "Hello World";
  };
}

export { createHelloWorld };
