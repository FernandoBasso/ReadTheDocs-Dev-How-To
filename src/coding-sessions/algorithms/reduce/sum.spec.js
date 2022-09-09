const { sum } = require("./sum-v3");

describe("sum()", () => {
  describe("when input is empty array", () => {
    it("should sum empty array to zero", () => {
      expect(sum([])).toEqual(0);
    });

    it("should sum arrays of one element", () => {
      expect(sum([0])).toEqual(0);
      expect(sum([-1, -5])).toEqual(-6);
    });

    it("should sum arrays of two or more elements", () => {
      expect(sum([-1, -2, -3])).toEqual(-6);
      expect(sum([1e3, -1, -2, -3])).toEqual(994);
    });
  });
});
