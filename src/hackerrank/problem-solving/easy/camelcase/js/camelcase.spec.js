//
// • design tool
// • documentation
// • unit test
// • regression test
//
//
// Garbage in, Garbage out (GIGO).
//

import { camelcase } from './camelcase-v3.js';

describe('camelcase(s)', () => {
  describe('when the input is an empty string', () => {
    it('should return 0 as there are no words', () => {
      expect(camelcase('')).toBe(0);
    });
  });

  describe('when the input does not have uppercase letters', () => {
    it('should return 0 as there are no words', () => {
      expect(camelcase('z')).toBe(1);
      expect(camelcase('one')).toBe(1);
      expect(camelcase('quintessential')).toBe(1);
    });
  });

  describe('when the input contains one or more uppercase letters', () => {
    it('should return the correct count', () => {
      expect(camelcase('oneTwo')).toEqual(2);
      expect(camelcase('firstName')).toEqual(2);
      expect(camelcase('yZ')).toBe(2);

      expect(camelcase('oneTwoThree')).toBe(3);
      expect(camelcase('getUserId')).toBe(3);
      expect(camelcase('xYZ')).toBe(3);

      expect(camelcase('oneTwoThreeFour')).toBe(4);
      expect(camelcase('getInvalidUserId')).toBe(4);
      expect(camelcase('wXYZ')).toBe(4);
    });
  });
});
