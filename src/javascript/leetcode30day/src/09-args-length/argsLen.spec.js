import { argsLen } from "./argsLen_v2";

describe("argsLen()", () => {
  it("should handle no arguments", () => {
    expect(argsLen()).toEqual(0);
  });

  it("should handle one argument", () => {
    expect(argsLen(undefined)).toEqual(1);
    expect(argsLen(null)).toEqual(1);
    expect(argsLen("hello")).toEqual(1);
    expect(argsLen([-42])).toEqual(1);
  });

  it("should handle two or more arguments", () => {
    expect(argsLen(undefined, null)).toEqual(2);
    expect(argsLen(null, null, [[[]]])).toEqual(3);
    expect(argsLen({ hello: "world" }, 2, 3, "four")).toEqual(4);
  });
});

argsLen()
