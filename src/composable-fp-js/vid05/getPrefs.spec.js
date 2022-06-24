import { getPrefs } from './getPrefs.js';
import defaultPrefs from './getPrefs-default-prefs.json';

describe('getPrefs()', () => {
  describe('when user is on free tier', () => {
    it('should get default preferences', () => {
      const user = {
        id: 2,
        name: 'Obi-wan Kenobi',
        premium: false,
      };

      expect(getPrefs(user)).toEqual(defaultPrefs);
    });
  });

  describe('when user is premium', () => {
    it('should get user preferences', () => {
      const user = {
        id: 1,
        name: 'Aayla Secura',
        premium: true,
        prefs: {
          theme: 'light',
          layout: 'column',
        }
      };

      expect(getPrefs(user)).toEqual(user.prefs);
    });
  });
});
