import { assertEquals } from "/deps.ts"
import { splitInt } from "./splitInt_v2.ts";

Deno.test("splitInt()", async (t) => {
  await t.step("should work for 10 and 1", () => {
    assertEquals(splitInt(10, 1), [10]);
  });

  await t.step("should work for 10 and 2", () => {
    assertEquals(splitInt(10, 2), [5, 5]);
  });

  await t.step("should work for 20 and 6", () => {
    assertEquals(splitInt(20, 6), [3, 3, 3, 3, 4, 4]);
  });

  await t.step("should work for 9 and 6", () => {
    assertEquals(splitInt(9, 6), [1, 1, 1, 2, 2, 2]);
  });
});
