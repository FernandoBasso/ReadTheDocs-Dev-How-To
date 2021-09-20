import { sum } from './sum';

describe('sum()', () => {
  it('should work', () => {
    expect(sum(1, -1)).toEqual(0);
  });
});
