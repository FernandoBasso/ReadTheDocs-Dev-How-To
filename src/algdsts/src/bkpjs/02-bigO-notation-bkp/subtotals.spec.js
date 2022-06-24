import { subtotals } from './subtotals-v2';

describe('subtotals()', () => {
  it('should produce the correct subtotals', () => {
    [
      [[0, 1, 2, 3], [0, 0 + 1, 0 + 1 + 2, 0 + 1 + 2 + 3]],
      [[10, 20, 30], [10, 10 + 20, 10 + 20 + 30]],
    ].forEach(([input, output]) => {
      expect(subtotals(input)).toEqual(output);
    });
  });
});
