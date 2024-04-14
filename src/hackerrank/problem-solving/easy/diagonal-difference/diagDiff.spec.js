const { diagDiff } = require('./diagDiff-v1');

describe('diagDiff()', () => {
  test('empty matrix', () => {
    expect(diagDiff([])).toEqual(0);
  });

  test('1x1 matrix', () => {
    const m1 = [
      [0],
    ];

    const m2 = [
      [-7],
    ];

    const m3 = [
      [99]
    ];

    expect(diagDiff(m1)).toEqual(0);
    expect(diagDiff(m2)).toEqual(0);
    expect(diagDiff(m3)).toEqual(0);
  });

  test('2x2 matrix', () => {
    const m1 = [
      [7, -1],
      [0, 0],
    ];

    expect(diagDiff(m1)).toEqual(8);
  });
});
