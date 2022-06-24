import { arrProduct } from './arrProduct-v1';

describe('arrProduct()', () => {
  it('should return 0 when array is empty', () => {
    expect(arrProduct([])).toEqual(1);
  });

  it('should return n when array has a single element n', () => {
    expect(arrProduct([0])).toEqual(0);
    expect(arrProduct([1])).toEqual(1);
    expect(arrProduct([2e5])).toEqual(2e5);
  });

  it('should add all the elements of the array', () => {
    expect(arrProduct([-3, 3])).toEqual(-3 * 3); // -9
    expect(arrProduct([-3, 3, 1])).toEqual(-3 * 3 * 1); // -9
    expect(arrProduct([-3, 3, -1])).toEqual(-3 * 3 * -1); // 9

    expect(
      arrProduct([1, 1, 1, 1, 1]),
    ).toEqual(1 * 1 * 1 * 1 * 1) // 1
    ;

    expect(
      arrProduct([5, 1, 2]),
    ).toEqual(5 * 1 * 2) // 10
    ;

    expect(
      arrProduct([5, 1, 0, 2]),
    ).toEqual(5 * 1 * 0 * 2) // 0
    ;
  });
});
