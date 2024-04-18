/**
 * @param {unknown} actual
 * @returns {{
 *  toBe: (expected: unknown) => true | never;
 *  notToBe: (expected: unknown) => true | never;
 * }}
 */
function myExpect(actual) {
  return {
    /**
     * Checks whether `actual === expected`.
     *
     * @param {unknown} expected
     * @throws {Error}
     * @returns {true}
     */
    toBe: function toBe(expected) {
      if (expected !== actual)
        throw Error("Not Equal");

      return true;
    },

    /**
     * Checks whether `actual !== expected`.
     *
     * @param {unknown} expected
     * @throws {Error}
     * @returns {true}
     */
    notToBe: function notToBe(expected) {
      if (expected === actual)
        throw Error("Equal");

      return true;
    },
  };
}

export { myExpect };
