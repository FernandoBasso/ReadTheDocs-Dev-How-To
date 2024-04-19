/**
 * Returns an object with with a few counter-related methods.
 *
 * - T.C: O(1).
 * - S.C: O(1).
 *
 * @param {number} init The initial integer value for the counter.
 * @returns {{
 *   increment: () => number;
 *   decrement: () => number;
 *   reset: () => number;
 * }}
 */
function createCounter(init) {
  var count = init;

  return {
    /**
     * - T.C: O(1).
     * - S.C: O(1).
     *
     * @returns {number}
     */
    increment: function increment() {
      return ++count;
    },

    /**
     * - T.C: O(1).
     * - S.C: O(1).
     *
     * @returns {number}
     */
    decrement: function decrement() {
      return --count;
    },

    /**
     * - T.C: O(1).
     * - S.C: O(1).
     *
     * @returns {number}
     */
    reset: function reset() {
      return count = init;
    },
  };
}

export { createCounter };
