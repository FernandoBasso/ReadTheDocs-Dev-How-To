import { maxSubArrSum } from './maxSubArrSum-v1';

describe("maxSubArrSum()", () => {
  it("should return the correct sum with valid input arrays", () => {
    expect(maxSubArrSum([0], 1)).toEqual(0);
    expect(maxSubArrSum([1, 2, 4, 2, 3], 2)).toEqual(4 + 2);

    expect(maxSubArrSum([1, 2, 3, -4, 7], 2)).toEqual(2 + 3);

    expect(
      maxSubArrSum([4, 2, 1, 6, 2], 4),
    ).toEqual(4 + 2 + 1 + 6); // 13

    expect(
      maxSubArrSum([-2, -4, -1, 0, 0], 3),
    ).toEqual(-1 + 0 + 0); // -1

    expect(
      maxSubArrSum([-2, -1, -1, -1, -2, -2, -2, -5], 3),
    ).toEqual(-1 + -1 + -1);
  });
});
