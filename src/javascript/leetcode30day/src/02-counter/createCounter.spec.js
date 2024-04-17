import { createCounter } from "./createCounter_v1";

describe("createCounter()", () => {
  it("should return a function", () => {
    const counter = createCounter(0);
    expect(counter.constructor).toEqual(Function);
  });

  it("should return the number passed on first call", () => {
    const counter = createCounter(-7);
    expect(counter()).toEqual(-7);
  });

  it("should return increments after first call", () => {
    const counter = createCounter(0);
    expect([
      counter(),
      counter(),
      counter(),
    ]).toEqual([0, 1, 2]);
  })
});
