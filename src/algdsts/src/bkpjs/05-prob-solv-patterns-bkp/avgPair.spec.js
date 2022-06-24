import { avgPair } from './avgPair-v1';

describe('avgPair()', () => {
  it('should find no average pair and return false', () => {
    expect(avgPair([])).toBe(false);
    expect(avgPair([1, 2, 3, 4, 6, 7], 1)).toBe(false);
    expect(avgPair([1, 2, 3, 4, 6, 7], 5.3)).toBe(false);
    expect(avgPair([-1, 0, 3, 4, 5, 6], 4.1)).toBe(false);
  });

  it('should find average pair and return true', () => {
    expect(avgPair([1, 2], 1.5)).toBe(true);
    expect(avgPair([1, 2, 3], 2.5)).toBe(true);
    expect(avgPair([-2, -1, 0, 1, 2], -1.5)).toBe(true);
    expect(
      avgPair([1, 3, 3, 5, 6, 7, 10, 12, 19], 8),
    ).toBe(true);
  });
});
