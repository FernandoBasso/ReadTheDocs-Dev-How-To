//
// CommonJS (node, module.exports, require).
// ES Modules (import, export).
//

const { add1 } = require('./add1-v1');

describe('add1()', () => {
  it('should add to 0', () => {
    expect(add1(0)).toEqual(1);
  });

  it('should add to -1', () => {
    expect(add1(-1)).toEqual(0);
  });

  it('should add to 10', () => {
    expect(add1(10)).toEqual(11);
  });
});
