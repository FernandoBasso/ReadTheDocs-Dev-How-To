import { isEven } from './isEven';

describe('isEven()', () => {
  it('should return true for even numbers', () => {
    [0, 2, -42, 1e3].forEach((input) => {
      expect(isEven(input)).toBe(true);
    });
  });

  it('should return false for non numbers', () => {
    [1, 3, -41, 1e3 - 1].forEach((input) => {
      expect(isEven(input)).toBe(false);
    });
  });
});
