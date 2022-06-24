import { same } from './same-v2';

describe('same()', () => {
  describe('when any value in a1 lack a matching vlaue in a2', () => {
    it('should return false', () => {
      // 1 squared is 1, but it is not in the second array.
      expect(same([1, 2, 3], [4, 9])).toBe(false);

      // Just because 4 is in the second array, it doesn't mean the
      // results have the same frequency. 4 should appear twice in the
      // second array because 2 appears twice in the first array. 9
      // should appear once in the second array.
      expect(same([2, 3, 2], [4, 9, 9])).toBe(false);
    });
  });

  describe('when all values in a1 have matching values in a2', () => {
    it('should return true', () => {
      expect(same([1, 2, 3], [1, 4, 9])).toBe(true);
      expect(same([1, 2, 3], [9, 1, 4])).toBe(true);
    });
  });
});
