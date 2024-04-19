import { createCounter } from "./createCounter_v1";

describe("createCounter()", () => {
  it("should return the object with increment()", () => {
    const counter = createCounter(0);

    expect(counter.increment()).toEqual(1);
  });

  it("should return the object with decrement()", () => {
    const counter = createCounter(0);

    expect(counter.decrement()).toEqual(-1);
  });

  it("should return the object with reset()", () => {
    const counter = createCounter(0);

    counter.decrement();
    counter.decrement();

    expect(counter.reset()).toEqual(0);

    expect([
      counter.increment(),
      counter.reset(),
      counter.decrement(),
    ]).toEqual([1, 0, -1]);
  });
});
