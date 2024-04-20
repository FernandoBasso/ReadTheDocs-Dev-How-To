import { filter } from "./filter_v1";

describe("filter()", () => {
  it("should return empty array for empty array input", () => {
    expect(filter([], e => e)).toEqual([]);
  });

  it("should filter based on the predicate", () => {
    const isEven = x => x % 2 === 0;

    expect(
      filter([1, 2, 3, 4], isEven)
    ).toEqual([2, 4]);
  });

  it("should use pass the index to the predicate", () => {
    const idxGt3 = (_x, i) => i > 3;

    expect(
      filter([1, 2, 3, 4], idxGt3)
    ).toEqual([]);

    expect(
      filter([1, 2, 3, 4, 5, 6], idxGt3)
    ).toEqual([5, 6]);
  });
});
