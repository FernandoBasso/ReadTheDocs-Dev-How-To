import { assertEquals } from "/deps.ts";
import { countAlphaNum } from './countAlphaNum-v1.ts';

Deno.test("countAlphaNum()", async (t) => {
  await t.step("should return an empty object if input is empty string", () => {
    assertEquals(countAlphaNum(""), {});
    assertEquals(countAlphaNum(new String("")), {});
  });
});

describe("countAlphaNum()", () => {
  it("should return an empty object if input is empty string", () => {
    expect(countAlphaNum("")).toEqual({});
    expect(countAlphaNum(new String(""))).toEqual({});
  });

  it("should work with simple lowercase char strings", () => {
    expect(countAlphaNum("zz")).toEqual({ z: 2 });
    expect(countAlphaNum("xyzxyx")).toEqual({ x: 3, y: 2, z: 1 });
  });

  it("should work with strings containing lowercase and uppercase chars", () => {
    expect(countAlphaNum("zzZ")).toEqual({ z: 3 });
    expect(countAlphaNum("xYzXyx")).toEqual({ x: 3, y: 2, z: 1 });
  });

  it("should work with alphanumeric strings", () => {
    expect(
      countAlphaNum("xYzX7Z57z"),
    ).toEqual({ x: 2, y: 1, z: 3, 7: 2, 5: 1 });
  });

  it("should work with any complex string", () => {
    expect(
      countAlphaNum("404 Errors!"),
    ).toEqual({ 4: 2, 0: 1, e: 1, r: 3, o: 1, s: 1 });

    expect(
      countAlphaNum("Execute order 66!"),
    ).toEqual({
      e: 4,
      x: 1,
      c: 1,
      u: 1,
      t: 1,
      o: 1,
      r: 2,
      d: 1,
      6: 2,
    });
  });
});
