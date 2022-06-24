import { sumRange } from './sumRange-v4';

describe('sumRange()', () => {
  it('should return the correct sum', () => {
    expect(sumRange(0)).toEqual(0);
    expect(sumRange(1)).toEqual(1);
    expect(sumRange(2)).toEqual(1 + 2);
    expect(sumRange(3)).toEqual(1 + 2 + 3);
    expect(sumRange(4)).toEqual(1 + 2 + 3 + 4);
    expect(sumRange(7)).toEqual(1 + 2 + 3 + 4 + 5 + 6 + 7);
  });
});
