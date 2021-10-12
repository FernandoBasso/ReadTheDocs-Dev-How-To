import { getPort } from './vid04d.js';

describe('getPort()', () => {
  describe('when file path is invalid', () => {
    it('should return default port', () => {
      expect(getPort('./wrong-path-config.json')).toEqual(3000);
    });
  });

  describe('when json is invalid', () => {
    it('should return default port', () => {
      expect(getPort('./vid04/config-04d-invalid.json')).toEqual(3000);
    });
  });

  describe('when path and json are both valid', () => {
    it('should return port as defined in config file', () => {
      expect(getPort('./vid04/config-04d-valid.json')).toEqual(8888);
    });
  });
});
