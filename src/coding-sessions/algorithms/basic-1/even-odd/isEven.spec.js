const { isEven } = require('./isEven-v7');

describe('isEven(n)', () => {
  it('should return true for 2', () => {
    expect(isEven(2)).toEqual(true);
  });
  
  it('should return true for 1444', () => {
    expect(isEven(1444)).toEqual(true);
  });
  
  it('should return false for 1', () => {
    expect(isEven(1)).toEqual(false);
  });
  
  it('should return false for 1443', () => {
    expect(isEven(1443)).toEqual(false);
  });
  
  it('should return false for -4', () => {
    expect(isEven(-4)).toEqual(true);
  });
  
  it('should return false for 1443', () => {
    expect(isEven(-3)).toEqual(false);
  });
});
