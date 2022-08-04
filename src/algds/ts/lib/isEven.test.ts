import { assertEquals } from "/deps.ts";
import { isEven } from "./isEven.ts";

Deno.test("isEven()", async (t) => {
  await t.step("should be true for 0, 2, -42, 1e3", () => {
    assertEquals(isEven(0), true);
    assertEquals(isEven(2), true);
    assertEquals(isEven(-42), true);
    assertEquals(isEven(1e3), true);
  });

  await t.step("should return false for 1, 3, -41, 1e3 - 1", () => {
    assertEquals(isEven(1), false);
    assertEquals(isEven(3), false);
    assertEquals(isEven(-41), false);
    assertEquals(isEven(1e3 - 1), false);
  });
});
