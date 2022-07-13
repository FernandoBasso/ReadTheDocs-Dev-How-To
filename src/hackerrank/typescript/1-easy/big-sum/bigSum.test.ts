import { assertEquals } from "/deps.ts";
import { sum } from "./bigSum-v1.ts";

Deno.test("sum()", async (t) => {
  await t.step("should sum empty array to zero", () => {
    assertEquals(sum([]), 0);
  });

  await t.step("should sum single-element arrays", () => {
    assertEquals(sum([0]), 0);
    assertEquals(sum([1]), 1);
    assertEquals(sum([-0]), 0);
    assertEquals(sum([-42]), -42);
    assertEquals(sum([1e4]), 1e4);
    assertEquals(sum([-1e4]), -1e4);
    assertEquals(sum([-1e-3]), -1e-3);
  });
});
