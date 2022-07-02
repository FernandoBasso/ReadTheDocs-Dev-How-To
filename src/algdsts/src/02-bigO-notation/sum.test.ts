import { assertEquals } from "/deps.ts";
import { sum } from "./sum-v3.ts";

Deno.test("sum()", async (t) => {
  await t.step("should add nothing", () => {
    assertEquals(sum([]), 0);
  });

  await t.step('should sum a few numbers', () => {
    assertEquals(sum([0]), 0);
    assertEquals(sum([1]), 1);
    assertEquals(sum([-1, -5]), -6);
    assertEquals(sum([-1, 1, -2, 2]), 0);
    assertEquals(sum([1, 2, 3, 4, 1e2]), 110)
  });
});

