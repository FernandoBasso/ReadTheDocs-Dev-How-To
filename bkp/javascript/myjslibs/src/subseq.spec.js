import {
  longest,
  strToMap,
  exists,
  nextIndex,
  isSubsequence,
} from './subseq';

describe('subseq.js', () => {
  describe('longest()', () => {
    it('should find no longest string', () => {
      expect(longest()).toEqual(null);
      expect(longest([])).toEqual(null);
    });

    it('should return the string when it is the only one', () => {
      expect(longest(['Quintessential'])).toEqual('Quintessential');
    });

    it('should return the longest of many', () => {
      expect(
        longest(['may', 'the', 'force', 'be', 'with', 'you'])
      ).toEqual('force');
    });
  });

  describe('strToMap()', () => {
    it('should handle single letter strings', () => {
      expect(strToMap('z')).toEqual({z: [0]});
    });

    it('should handle non-repeating letters', () => {
      expect(
        strToMap('thing')
      ).toEqual({t: [0], h: [1], i: [2], n: [3], g: [4]});
    });

    it('should handle repeating letters', () => {
      expect(
        strToMap('racecar')
      ).toEqual({r: [0, 6], a: [1, 5], c: [2, 4], e: [3]});

      expect(
        strToMap('ruuun')
      ).toEqual({r: [0], u: [1, 2, 3], n: [4]});
    });
  });

  describe('exists()', () => {
    it('should not find any existing letter in the map', () => {
      expect(
        exists('js', { c: [0], s: [1, 2]})
      ).toBe(false);
    });
  });

  describe('nextIndex()', () => {
    it('should return false because all indices are less and min index', () => {
      expect(
        nextIndex([1, 3], 5)
      ).toBe(false);
    });

    it('should return 0 + 1', () => {
      expect(nextIndex([0, 3, 4], 0)).toEqual(0 + 1);
    });
  });

  describe('isSubsequence()', () => {
    it('should not match as a subsequence', () => {
      expect(
        isSubsequence('xs', 'bummer')
      ).toBe(false);
    });

    it('should match as a subsequence', () => {
      expect(
        isSubsequence('ban', strToMap('banana'))
      ).toBe(true);

      expect(
        isSubsequence('yup', strToMap('syrup'))
      ).toBe(true);
    });
  });


});

