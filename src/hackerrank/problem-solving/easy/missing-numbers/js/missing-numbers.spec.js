import { missingNums } from './missing-numbers-v4.js';

//
// Think of “tests” like this:
//
// • design tool
// • documentation
// • testing
// • regression testing
//

describe('missingNums(arr, brr)', () => {
  describe('when the two inputs are empty', () => {
    it('should return an empty array', () => {
      expect(missingNums([], [])).toEqual([]);
    });
  });

  describe('when brr is smaller than arr', () => {
    it('should return an empty array', () => {
      expect(missingNums([1, 2], [])).toEqual([]);
      expect(missingNums([1, 2], [1])).toEqual([]);
      expect(missingNums([1, 2], [2])).toEqual([]);
    });
  });

  describe('when the two arrays have the same elements', () => {
    it('should return an empty array !!!', () => {
      var arr = [1, 2, 3];
      var brr = [1, 2, 3];

      expect(missingNums(arr, brr)).toEqual([]);

      var arr2 = [1, 3, 2];
      var brr2 = [2, 1, 3];

      expect(missingNums(arr2, brr2)).toEqual([]);
    });
  });

  describe('when there is a missing number', () => {
    it('should return the array with that missing number', () => {
      var arr = [1, 2];
      var brr = [1, 2, 3];

      expect(missingNums(arr, brr)).toEqual([3]);
    });
  });

  describe('when all the numbers are missing arr', () => {
    it('should return all the numbers in the brr', () => {
      var arr = [];
      var brr = [1, 2, 3];

      expect(missingNums(arr, brr)).toEqual(brr);

      var arr2 = [1, 2, 3];
      var brr2 = [4, 5, 6];

      expect(missingNums(arr2, brr2)).toEqual(brr2);
    });
  });

  describe('when there is a missing number with different frequencies', () => {
    it('should return the array with that missing number', () => {
      var arr = [1, 2, 3];
      var brr = [1, 2, 3, 3, 3, 3];

      expect(missingNums(arr, brr)).toEqual([3]);

      var arr = [3, 2, 1];
      var brr = [3, 3, 1, 3, 2, 3];

      expect(missingNums(arr, brr)).toEqual([3]);
    });
  });

  describe('input example from HackerRank', () => {
    it('should return sorted array', () => {
      var arr = [203, 204, 205, 206, 207, 208, 203, 204, 205, 206];
      var brr = [203, 204, 204, 205, 206, 207, 205, 208, 203, 206, 205, 206, 204];

      expect(missingNums(arr, brr)).toEqual([204, 205, 206]);
    });

    it('should sort by the number values', () => {
      var arr = [11, 4, 11, 7, 13, 4, 12, 11, 10, 14];
      var brr = [11, 4, 11, 7, 3, 7, 10, 13, 4, 8, 12, 11, 10, 14, 12];

      expect(missingNums(arr, brr)).toEqual([3, 7, 8, 10, 12]);
    });
  });
});

