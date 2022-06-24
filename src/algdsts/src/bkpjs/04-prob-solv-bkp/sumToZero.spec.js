import { sumToZero } from './sumToZero-v2';

describe('sumToZero()', () => {
  it('should return undefined when no pair sum to zero', () => {
    expect(sumToZero([0])).toBe(undefined);
    expect(sumToZero([-1, 2])).toBe(undefined);
    expect(sumToZero([-2, -1, -3])).toBe(undefined);
    expect(sumToZero([1, 2, -3, 4])).toBe(undefined);
  });

  // See <1> in sum-zero.mjs.
  it('should return undefined for false positives', () => {
    expect(sumToZero([-1, 0, 2])).toBe(undefined);
  });

  it('should return the pair a pair sums to zero', () => {
    expect(sumToZero([0, 0])).toEqual([0, 0]);
    expect(sumToZero([1, -1])).toEqual([1, -1]);
    expect(sumToZero([1, 2, -3, 4])).toBe(undefined);
  });
});
