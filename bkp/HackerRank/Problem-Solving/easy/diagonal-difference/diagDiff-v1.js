let squareMatrix = [
  [1, 2, 3],
  [4, 5, 6],
  [9, 8, 9],
];

let squareMatrix2 = [
  [1, 2, 3, 10],
  [4, 5, 6, 10],
  [9, 8, 9, 10],
  [11, 12, 13, 10],
];

const log = console.log.bind(console);

function diagDiff(sqrMatrix) {
  const len = sqrMatrix.length;
  let ltrDiag = 0;
  let rtlDiag = 0;

  for (let i = 0; i < len; i++) {
    for (let j = 0; j < len; j++) {
      if (i === j)
        ltrDiag += sqrMatrix[i][j];

      if (i + j === len - 1)
        rtlDiag += sqrMatrix[i][j];
    }
  }

  return Math.abs(ltrDiag - rtlDiag);
};

module.exports = { diagDiff };
