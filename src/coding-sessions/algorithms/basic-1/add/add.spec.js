const { add } = require('./add-v1');

describe('add(x, y)', () => {
  it('should add 0 and 0', () => {
    expect(add(0, 0)).toEqual(0);
  });

  it('should add 1 and 1', () => {
    expect(add(1, 1)).toEqual(2);
  });

  it('should add with negatives', () => {
    expect(add(-1, -1)).toEqual(-2);
  });
  
  it('should add with negatives and positives', () => {
    expect(add(-3, 2)).toEqual(-1);
  });
});
