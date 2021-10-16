// import { getStreetName } from './getStreetName-v1.js';
// import { getStreetName } from './getStreetName-v2.js';
// import { getStreetName } from './getStreetName-v3.js';
import { getStreetName } from './getStreetName-v4.js';

describe('getStreetName()', () => {
  describe('when address is empty', () => {
    it('should return "no street"', () => {
      const address = undefined;
      const user = { address };
      expect(getStreetName(user)).toEqual('no street');
    });
  });

  describe('when address object exists but street is empty', () => {
    it('should return "no street"', () => {
      const user1 = {
        name: 'User 1',
        address: {
          id: 1,
          /* Note no 'street' property */
        },
      };

      const user2 = {
        name: 'User 2',
        address: {
          street: undefined,
        }
      };

      [user1, user2].forEach(user => {
        expect(getStreetName(user)).toEqual('no street');
      });
    });
  });

  describe('when address and street exist', () => {
    it('should return the street name', () => {
      const user1 = {
        name: 'User 1',
        address: {
          street: {
            name: 'Linux Torvalds Linux Avenue',
          },
        },
      };

      const user2 = {
        name: 'User 1',
        address: {
          street: {
            name: 'Braam Moolenaar Road of Vim',
          },
        },
      };

      [user1, user2].forEach(user => {
        expect(getStreetName(user)).toEqual(user.address.street.name);
      });
    });
  });
});
