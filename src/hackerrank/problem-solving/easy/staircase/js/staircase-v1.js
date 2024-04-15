/**
 * Prints an ascii-art of a stair case of `n` steps.
 *
 * @param {number} n The number of steps to print.
 * @sig Number -> Void
 */
function staircase(n) {
  for (var i = 1; i <= n; ++i) {
    var row = new Array(n).fill(' ').fill('#', n - i).join('');
    console.log(row);
  }
}

staircase(24);

export { staircase };
