const { isOdd } = require('./isOdd-v3');

describe('isOdd(n)', () => {
  it('should return false for 2', () => {
    expect(isOdd(2)).toEqual(false);
  });
  
  it('should return false for 1444', () => {
    expect(isOdd(1444)).toEqual(false);
  });
  
  it('should return true for 1', () => {
    expect(isOdd(1)).toEqual(true);
  });
  
  it('should return true for 1443', () => {
    expect(isOdd(1443)).toEqual(true);
  });
  
  it('should return false for -4', () => {
    expect(isOdd(-4)).toEqual(false);
  });
  
  it('should return true for -1443', () => {
    expect(isOdd(-1443)).toEqual(true);
  });
});
