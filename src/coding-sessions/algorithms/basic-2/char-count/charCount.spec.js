const { charCount } = require('./charCount-v4');

describe('charCount()', () => {
  it('should work with lowercase alphabetic strings', () => {
    expect(charCount('hello')).toEqual({
      h: 1,
      e: 1,
      l: 2,
      o: 1,
    });
  }) ;

  it('should handle input in a case insensitive way', () => {
    expect(charCount('Racecar')).toEqual({
      r: 2, // R and r.
      a: 2,
      c: 2,
      e: 1,
    });
  });

  it('should ignore non-alphanumeric chars', () => {
    //
    // Should ignore the space " " and "!".
    //
    expect(charCount('Hi there!')).toEqual({
      h: 2,
      i: 1,
      t: 1,
      e: 2,
      r: 1,
    });
  });

  it('should work with strings including digits', () => {
    expect(charCount('3-way handshake (SYN, SYN-ACK, ACK)')).toEqual({
      3: 1,
      a: 5,
      c: 2,
      d: 1,
      e: 1,
      h: 2,
      k: 3,
      n: 3,
      s: 3,
      w: 1,
      y: 3,
    });
  });
});