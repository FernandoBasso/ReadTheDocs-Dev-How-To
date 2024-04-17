/**
 * @param {unknown} val
 * @returns {{
 *  toBe: (v: unknown) => boolean
 * }}
 */
function myExpect(val) {
  return {
    toBe: function toBe(v) {
      return v === val;
    },
    notToBe: function notToBe(v) {
      return v !== val;
    },
  };
}

export { myExpect };
