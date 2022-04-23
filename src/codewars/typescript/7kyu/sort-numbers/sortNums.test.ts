import { assertEquals } from "/deps.ts";
import { sortNums } from "./sortNums-v1.ts";

Deno.test("sortNums()", async (t) => {
  await t.step("should return empty array", () => {
    assertEquals(sortNums(), []);
    assertEquals(sortNums(undefined), []);
    assertEquals(sortNums(null), []);
    assertEquals(sortNums([]), []);
  });

  await t.step("should return array with the sole element", () => {
    assertEquals(sortNums([1]), [1]);
    assertEquals(sortNums([-42]), [-42]);
    assertEquals(sortNums([1 / 2]), [1 / 2]);
  });

  await t.step("should return numbers in ascending order", () => {
    assertEquals(sortNums([-3, -42, 0]), [-42, -3, 0]);
    assertEquals(sortNums([-3, 1 / 2, -42, 0]), [-42, -3, 0, 1 / 2]);
    assertEquals(sortNums([-3, -42, Infinity, 0, -Infinity]), [
      -Infinity,
      -42,
      -3,
      0,
      Infinity,
    ]);
  });
});
