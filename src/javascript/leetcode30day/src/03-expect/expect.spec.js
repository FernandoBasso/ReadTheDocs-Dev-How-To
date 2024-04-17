import { myExpect } from "./expect_v1";

describe("expect()", () => {
  it("implements toBe() for literals", () => {
    expect(myExpect(1).toBe(1)).toEqual(true);
    expect(myExpect(1).toBe(2)).toEqual(false);
  });

  it("implements notToBe() for literals", () => {
    expect(myExpect(1).notToBe(1)).toEqual(false);
    expect(myExpect(1).notToBe(2)).toEqual(true);
  });
});
