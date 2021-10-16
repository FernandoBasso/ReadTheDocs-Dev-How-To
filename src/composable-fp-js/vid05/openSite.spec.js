// import { openSite } from './openSite-v1.js';
import { openSite } from './openSite-v2.js';

describe('openSite()', () => {
  it('should display login page when user does not exist', () => {
    expect(openSite(undefined)).toEqual('Sign In!');
  });

  it('should display home page when user exists', () => {
    expect(
      openSite({ id: 1, name: 'Ahsoka Tano' })
    ).toEqual('Welcome to our site!');
  });
});

