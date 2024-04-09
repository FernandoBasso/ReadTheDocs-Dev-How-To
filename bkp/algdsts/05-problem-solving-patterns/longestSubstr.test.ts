import { findLongestSubstr } from './longestSubstr-v2';

describe('findLongestSubstr()', () => {
  describe('when input string is empty', () => {
    it('should return 0', () => {
      expect(findLongestSubstr('')).toEqual(0);
    });
  });

  describe('when string contains no distinct letters', () => {
    it('should return 1', () => {
      expect(findLongestSubstr('bbbbbb')).toEqual(1);
    });
  });

  describe('when string contains substring with distinct letters', () => {
    it('should return the length of the substring', () => {
      expect(findLongestSubstr('xyxabcdyx')).toEqual(6);
      expect(findLongestSubstr('rithmschool')).toEqual(7);
      expect(findLongestSubstr('thisisawesome')).toEqual(6);
      expect(findLongestSubstr('thecatinthehat')).toEqual(7);
      expect(findLongestSubstr('longestsubstring')).toEqual(8);
      expect(findLongestSubstr('thisishowwedoit')).toEqual(6);
    });
  });
});
