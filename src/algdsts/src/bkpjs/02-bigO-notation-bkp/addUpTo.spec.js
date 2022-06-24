import { addUpTo } from './addUpTo-v2';

describe('addUpTo()', () => {
  it('should add nothing', () => {
    expect(addUpTo(0)).toEqual(0);
  });

  it('should add a few numbers', () => {
    expect(addUpTo(1)).toEqual(1);
    expect(addUpTo(2)).toEqual(1 + 2);
    expect(addUpTo(3)).toEqual(1 + 2 + 3);
    expect(addUpTo(6)).toEqual(21);
    expect(addUpTo(99)).toEqual(4950);
  });
});

