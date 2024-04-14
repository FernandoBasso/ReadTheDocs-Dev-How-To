const log = console.log.bind(console);

/**
 * Calculates the diagonal difference of the
 * square matrix.
 *
 * @sig [Number] -> Number
 */
function diagDiff(xs) {
  let ltrDiag = 0;
  let rtlDiag = 0;
  let len = xs.length;

  for (let i = 0; i < len; ++i) {
    ltrDiag += xs[i][i];
    rtlDiag += xs[i][len - i - 1];
  }

  return Math.abs(ltrDiag - rtlDiag);
}

log(
  diagDiff([
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
  ]),
);

// log(
//   diagDiff([
//     [11, 2, 4],
//     [4,  5, 6],
//     [10, 8, -12],
//   ]),
// );
