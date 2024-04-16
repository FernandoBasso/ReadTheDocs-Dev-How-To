/**
 * Prints an ascii-art of a stair case of `n` steps.
 *
 * - T.C: O(n²)
 * - S.C: O(n).
 *
 * @param {number} n The number of steps to print.
 * @sig Number -> Void
 */
 function staircase(n) {
  let r = 1;

  while (r <= n) {
    let blanks = [...Array(n - r)].map(i => ' ');
    let hashes = [...Array(n - (n - r++))].map(i => '#');
    console.log([...blanks, ...hashes].join(''));
  }
}

export { staircase };
