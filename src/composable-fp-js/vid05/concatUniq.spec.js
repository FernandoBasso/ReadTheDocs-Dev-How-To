// import { concatUniq } from './concatUniq-v1.js';
import { concatUniq } from './concatUniq-v2.js';

describe('concatUniq()', () => {
  describe('when the array already contains the value', () => {
    it('should not append the value to the array', () => {
      expect(concatUniq(0, [0, 1, 2])).toEqual([0, 1, 2]);
      expect(concatUniq('z', ['x', 'y', 'z'])).toEqual(['x', 'y', 'z']);
    });
  });

  describe('when array does not contain the value', () => {
    it('should append the value to the array', () => {
      expect(concatUniq(0, [1, 2])).toEqual([1, 2, 0]);
      expect(concatUniq('y', ['x', 'z'])).toEqual(['x', 'z', 'y']);
    });
  });
});
