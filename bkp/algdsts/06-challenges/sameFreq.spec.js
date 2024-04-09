import { sameFreq } from './sameFreq-v1';

describe('sameFreq()', () => {
  it('should return false when frequencies do not match', () => {
    expect(sameFreq(12, 123)).toBe(false);
    expect(sameFreq(1, 2)).toBe(false);
    expect(sameFreq(123, 122)).toBe(false);
  });

  it('should return true when frequencies match', () => {
    expect(sameFreq(1, 1)).toBe(true);
    expect(sameFreq(123, 123)).toBe(true);
    expect(sameFreq(322, 232)).toBe(true);
    expect(sameFreq(
      203010509999991,
      993020501099199,
    )).toBe(true);
  });
});
