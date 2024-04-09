import { power } from './power-v1';

describe('power()', () => {
  it('should return 1 when raised to the 0th power', () => {
    expect(power(0, 0)).toEqual(1);
    expect(power(3e5, 0)).toEqual(1);
  });

  it('should return n when raised to 1st power', () => {
    expect(power(0, 1)).toEqual(0);
    expect(power(1, 1)).toEqual(1);
    expect(power(3e2, 1)).toEqual(3e2);
  });

  it('should raise n to the m power', () => {
    expect(power(3, 4)).toEqual(3 * 3 * 3 * 3); // 81
    expect(power(2, 5)).toEqual(2 * 2 * 2 * 2 * 2); // 32
    expect(power(2, 128)).toEqual(2 ** 128);
    expect(power(-2, 32)).toEqual((-2) ** 32);
  });
});
