import { sumFrom1To } from './sumFrom1To-v1';

describe('sumFrom1To()', () => {
  it('should return 1 when input is 1', () => {
    expect(sumFrom1To(1)).toEqual(1);
  });

  it('should return the correct sum from other numbers', () => {
    expect(sumFrom1To(3)).toEqual(3 + 2 + 1);
    expect(sumFrom1To(5)).toEqual(5 + 4 + 3 + 2 + 1);
  });
});
