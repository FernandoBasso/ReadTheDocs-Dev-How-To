import { odds as collectOdds } from './collectOdds-v3';

describe('collectOdds()', () => {
  describe('when input is empty', () => {
    it('should return empty array', () => {
      expect(collectOdds([])).toEqual([]);
    });
  });

  describe('when input contains no odds', () => {
    it('should return empty array', () => {
      expect(collectOdds([2])).toEqual([]);
      expect(collectOdds([2, 8, -4])).toEqual([]);
    });
  });

  describe('when array contains only odds', () => {
    it('should return an array with the same values as input', () => {
      expect(collectOdds([3])).toEqual([3]);
      expect(collectOdds([-5, -7, 9])).toEqual([-5, -7, 9]);
    });
  });
});
