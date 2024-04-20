import { map } from "./map_v1";

describe("map()", () => {
  it("should return empty array with empty array input", () => {
    expect(map([], () => null)).toEqual([]);
  });

  it("should apply the mapping function and return new, transformed array", () => {
    expect(map([1], x => x)).toEqual([1]);
    expect(map([1], x => x + 1)).toEqual([2]);
  });

  it("should not return the same array reference", () => {
    var xs = [1, 2, 3];
    var mapped = [1, 2, 3];
    expect(xs === mapped).not.toBe(true);
  });

  it("should return array transformed by function", () => {
    const negate = x => -1 * x;

    expect(
      map([1, 2, 3], negate)
    ).toEqual([-1, -2, -3]);

    const toUpper = s => s.toUpperCase();

    expect(
      map(["a", "b", "c"], toUpper)
    ).toEqual(["A", "B", "C"]);
  });

  it("should make use of the index parameter in the callback", () => {
    const plusOne = (n, i) => n + i;
    expect(
      map([1, 2, 3], plusOne)
    ).toEqual([1 + 0, 2 + 1, 3 + 2])
    //            1,    3  ,   5
  })
});
