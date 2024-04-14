const log = console.log.bind(console);

/**
 * Calculates the diagonal difference of the square matrix.
 *
 * @sig [Number] -> Number
 */
function diagDiff(xs) {
  let ltrDiag = 0;
  let rtlDiag = 0;
  const lastPos = xs.length - 1;

  for (let i = 0; i <= lastPos; ++i) {
    ltrDiag += xs[i][i];
    rtlDiag += xs[i][lastPos - i];
  }

  return Math.abs(ltrDiag - rtlDiag);
}

module.exports = { diagDiff };

// log(
//   diagDiff([
//     [1, 2, 3],
//     [4, 5, 6],
//     [6, 8, 9],
//   ]),
// );
//
// log(
//   diagDiff([
//     [1, 2, 3],
//     [4, 5, 6],
//     [9, 8, 9],
//   ]),
// );
