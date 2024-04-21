import { reduce } from "./reduce_v2";

describe("reduce()", () => {
  it("should return initial value when xs is empty", () => {
    expect(reduce([], () => null, 0)).toEqual(0);
    expect(reduce([], () => null, 1)).toEqual(1);
  });

  it("should return a valid accumulated result", () => {
    const sum = (acc, n) => acc + n;

    expect(
      reduce([1, 2, 3], sum, 0)
    ).toEqual(0 + 1 + 2 + 3);

    expect(
      reduce([1, 2, 3], sum, 10)
    ).toEqual(10 + 1 + 2 + 3);
  });

  it("should work with arrays result", () => {
    const incEach = (acc, x) => [...acc, x + 1];

    expect(
      reduce([0, 1, 2], incEach, [])
    ).toEqual([1, 2, 3]);
  });
});
