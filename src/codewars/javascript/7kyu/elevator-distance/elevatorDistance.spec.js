import { elevatorDistance } from './elevatorDistance-v1';

describe('elevatorDistance()', () => {
  it('should calculate two consecutive same floor', () => {
    expect(elevatorDistance([0, 0])).toEqual(0);
    expect(elevatorDistance([3, 3])).toEqual(0);
    expect(elevatorDistance([17, 17])).toEqual(0);
  });

  it('should calculate a few floors distance', () => {
    expect(elevatorDistance([1, 2, 3])).toEqual(2);
    expect(elevatorDistance([5, 2, 8])).toEqual(9);
  });

  it('should calculate lengthier floor distance', () => {
    expect(elevatorDistance([7, 1, 7, 1])).toEqual(18);
    expect(elevatorDistance([1, 5, 3, 2])).toEqual(7);
    expect(
      elevatorDistance([7, 1, 7, 1, 18, 15, 13, 1, 9, 19, 0, 3, 7])
    ).toEqual(96);
  });
});
