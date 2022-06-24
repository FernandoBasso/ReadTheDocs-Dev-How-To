import { fact } from './factorial-v1';

describe('fact()', () => {
  it('should return 1 when input is 1', () => {
    expect(fact(1)).toEqual(1);
  });

  it('should return 2 when input is 2', () => {
    expect(fact(2)).toEqual(2 * 1);
  });

  it('should return the factorial of 3', () => {
    expect(fact(3)).toEqual(3 * 2 * 1);
  });

  it('should return the factorial of 5', () => {
    expect(fact(5)).toEqual(5 * 4 * 3 * 2 * 1);
  });
});
