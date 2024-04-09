import { areThereDups } from './areThereDups-v4';

describe('areThereDups()', () => {
  it('should return false when no params are passed', () => {
    expect(areThereDups()).toBe(false);
  });

  it('should return false when there are no dups', () => {
    expect(areThereDups(1, 2, 3)).toBe(false);
  });

  it('should return true when there are duplicates', () => {
    expect(areThereDups(1, 2, 1)).toBe(true);
    expect(areThereDups(1, 2, 3, -1, -3 + 2)).toBe(true);
  });
});
