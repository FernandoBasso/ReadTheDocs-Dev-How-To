import {
  replace,
} from './lib-strings';

describe('strings.js', () => {

  describe('replace()', () => {
    it('should not crash', () => {
      expect(replace()).toEqual(undefined);
      expect(replace(null, null, {})).toEqual(undefined);
    });
  });

});
