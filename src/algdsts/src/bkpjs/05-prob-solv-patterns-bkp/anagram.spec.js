import { isAnagram } from './anagram-v1';

describe('isAnagram()', () => {
  it('should return false with inputs of different lengths', () => {
    expect(isAnagram('', 'z')).toBe(false);
    expect(isAnagram('listen', 'silen')).toBe(false);
  });

  it('should return true with empty string inputs', () => {
    expect(isAnagram('', '')).toBe(true);
    expect(isAnagram(new String(''), new String(''))).toBe(true);
  });

  it('should return true for single word, non space anagrams', () => {
    expect(isAnagram('listen', 'silent')).toBe(true);
  });
});
