import { indexOf } from './indexOf-v2';

describe('indexOf()', () => {
  it('should return -1 with empty arrays', () => {
    expect(indexOf(9, [])).toEqual(-1);
    expect(indexOf(9, new Array(0))).toEqual(-1);
  });

  it('should return index 0 when element is at position 0', () => {
    expect(indexOf(-9, [-9, 9, 7])).toEqual(0);
  });

  it('should return last index when elem is at the last index', () => {
    const nums = [-9, 7, 9];
    expect(indexOf(9, nums)).toEqual(nums.length - 1);
  });

  it('should return the index of any other position of elem', () => {
    expect(indexOf(-7, [-9, -7, -1, 0, 3, 5])).toEqual(1);
    expect(indexOf(0, [-9, -7, -1, 0, 3, 5])).toEqual(3);
    expect(indexOf(-1, [-9, -7, -1, 0, 3, 5])).toEqual(2);
  });
});
