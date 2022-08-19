const { capitalizeAll } = require('./capitalize-v2');

describe('capitalizeAll()', () => {
  it('should return empty string', () => {
    expect(capitalizeAll('')).toEqual('');
  });

  it('should work for single-letter words', () => {
    expect(capitalizeAll('a b c')).toEqual('A B C');
  });

  it('should work for two-or-more-letter words', () => {
    expect(capitalizeAll('hello world')).toEqual('Hello World');
  });
});
