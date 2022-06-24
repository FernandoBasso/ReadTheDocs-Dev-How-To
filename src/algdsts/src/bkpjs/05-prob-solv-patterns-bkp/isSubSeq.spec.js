import { isSubSeq } from './isSubSeq-v2';

describe('isSubSeq()', () => {
  it('should return true', () => {
    expect(isSubSeq('dc', 'abcd')).toBe(false);
    expect(isSubSeq('abc', 'acb')).toBe(false);
    expect(isSubSeq('abcde', 'abcd')).toBe(false);
    expect(isSubSeq('may the force', 'be with you')).toBe(false);
  });

  it('should return true', () => {
    expect(isSubSeq('force', 'may the force be with you!')).toBe(true);
    expect(isSubSeq('ana', 'banana')).toBe(true);
    expect(isSubSeq('abc', 'abracadabra')).toBe(true);
    expect(isSubSeq('sing', 'sting')).toBe(true);
  });
});
