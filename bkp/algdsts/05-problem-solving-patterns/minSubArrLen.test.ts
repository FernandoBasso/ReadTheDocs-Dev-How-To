import { minSubArrLen } from './minSubArrLen-v1';

describe("minSubArrLen()", () => {
  describe("when the input array is empty", () => {
    it("should return 0", () => {
      expect(minSubArrLen([], 1)).toEqual(0);
      expect(minSubArrLen(new Array(), 1)).toEqual(0);
      //
      // An empty array can't possibly sum to 1.
      //
    });
  });

  describe("when the entire array summed is not >= n", () => {
    it("should return 0", () => {
      expect(
        minSubArrLen([1, 4, 16, 22, 5, 7, 8, 9, 10], 95),
      ).toEqual(0);
      //
      // Even adding all the elements doesn't amount to 95.
      //
    });
  });

  describe("when a single element >= n", () => {
    it("should return 1", () => {
      expect(minSubArrLen([1, 13, 7], 13)).toEqual(1);
      //
      // 1 Because 13 >= 13.
      //

      expect(
        minSubArrLen([3, 1, 7, 11, 2, 9, 8, 21, 62, 33, 19], 52),
      ).toEqual(1);

      // 1 because 62 >= 52.
      //
    });
  });

  describe("when 2 elements summed >= n", () => {
    it("should return 2", () => {
      expect(
        minSubArrLen([2, 3, 1, 2, 4, 3], 7),
      ).toEqual(2);
      // 2 because [4, 3] is the smallest subarray that produces 7
      // when summed.

      expect(minSubArrLen([2, 1, 6, 5, 4], 9)).toEqual(2);
      // 2 because [5, 4] is the smallest subarray.

      minSubArrLen([4, 3, 3, 8, 1, 2, 3], 11);
      // 2 because 3 + 8 is 11.
    });
  });

  describe("when all the elements are required", () => {
    it("should return the length of the input array", () => {
      const nums3 = [1, 2, 3];
      expect(minSubArrLen(nums3, 6)).toEqual(nums3.length);

      const nums9 = [10, 20, 30, 40, 50, 60, 70, 80, 90];
      expect(minSubArrLen(nums9, 450)).toEqual(nums9.length);
    });
  });

  it("should handle random subarray lengths required", () => {
    minSubArrLen([1, 4, 16, 22, 5, 7, 8, 9, 10], 39);
    // 3

    minSubArrLen([1, 4, 16, 22, 5, 7, 8, 9, 10], 55);
    // 5
  });
});
