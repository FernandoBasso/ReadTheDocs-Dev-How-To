import { countUniqVals } from './countUniqVals-v2';

describe('countUniqVals()', () => {
  it('should return 0 with empty input', () => {
    expect(countUniqVals([])).toEqual(0);
    expect(countUniqVals(new Array(0))).toEqual(0);
  });

  it('should return the correct count of unique values', () => {
    expect(countUniqVals([0])).toEqual(1);
    expect(countUniqVals([-7])).toEqual(1);
    expect(countUniqVals([-3, -3, -3])).toEqual(1);
    expect(countUniqVals([-3, -3, 0, 1, 1, 1, 1, 1])).toEqual(3);
    expect(countUniqVals([-3, -2, -1, 0, 0, 1, 1])).toEqual(5);
    expect(countUniqVals([-3, -2, -1, 0, 1])).toEqual(5);
  });
});
