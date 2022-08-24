const { toUpper } = require('./toUpper-v1');
const { upperCaseAll } = require('./upperCaseAll-v1');

describe('toUpper()', () => {
  it('should work with empty strings', () => {
    expect(toUpper('')).toEqual('');
  });

  it('should work with single-char strings', () => {
    expect(toUpper('z')).toEqual('Z');
  });

  it('should uppercase the other strings', () => {
    expect(toUpper('x y z')).toEqual('X Y Z');
    expect(toUpper('abc')).toEqual('ABC');
    expect(toUpper('use the force')).toEqual('USE THE FORCE');
  });
});

describe('upperCaseAll()', () => {
  it('should uppercase all strings in the array', () => {
    expect(
      upperCaseAll(['read', 'the', 'source', 'luke'])
    ).toEqual(['READ', 'THE', 'SOURCE', 'LUKE']);
  });
});
