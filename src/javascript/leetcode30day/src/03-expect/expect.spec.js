import { myExpect } from "./expect_v1";

describe("expect()", () => {
  it("implements toBe() for literal types", () => {
    expect(myExpect(1).toBe(1)).toEqual(true);
    expect(() => myExpect(1).toBe(2)).toThrow("Not Equal");
  });

  it("implements toBe() for reference types", () => {
    var f = {};
    var g = f;
    expect(myExpect(f).toBe(g)).toEqual(true);
    expect(() => myExpect(f).toBe({})).toThrow("Not Equal");
  });

  it("implements notToBe() for literal types", () => {
    expect(() => myExpect(1).notToBe(1)).toThrow("Equal");
    expect(myExpect(1).notToBe(2)).toEqual(true);
  });

  it("implements notToBe() for literal types", () => {
    var f = {};
    var g = f;
    expect(() => myExpect(f).notToBe(g)).toThrow("Equal");
    expect(myExpect(f).notToBe({})).toEqual(true);
  });
});
