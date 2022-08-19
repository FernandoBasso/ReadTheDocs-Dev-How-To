const { add10 } = require("./add10-v1");

describe("add10()", () => {
  it("should add 10 to 0", () => {
    expect(add10(0)).toEqual(10);
  });

  it("should add 10 to -10", () => {
    expect(add10(-10)).toEqual(0);
  });
});
