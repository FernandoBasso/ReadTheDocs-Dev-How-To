import { assertEquals } from "/deps.ts";
import { doubleNums } from "./doubleNums-v1.ts";

Deno.test("doubleNums()", async (t) => {
  await t.step("should return empty when input is empty array", () => {
    assertEquals(doubleNums([]), []);
  });

  await t.step("should work on arrays of a single number", () => {
    assertEquals(doubleNums([0]), [0]);
    assertEquals(doubleNums([-3]), [-6]);
    assertEquals(doubleNums([1e3]), [2000]);
  });

  await t.step("should work on arrays of multiple numbers", () => {
    assertEquals(doubleNums([0, 0, -0]), [0, 0, -0]);
    assertEquals(doubleNums([-3, 3, 1e3, -1e3]), [-6, 6, 2000, -2000]);
  });
});
