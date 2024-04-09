import { assertEquals } from "/deps.ts";
import { addUpTo } from "./addUpTo-v1.ts";

Deno.test("addUpTo()", async (t) => {
  await t.step("should add nothing", () => {
    assertEquals(addUpTo(0), 0);
  });

  await t.step('should add a few numbers', () => {
    assertEquals(addUpTo(1), 1);
    assertEquals(addUpTo(2), 1 + 2);
    assertEquals(addUpTo(3), 1 + 2 + 3);
    assertEquals(addUpTo(6), 21);
    assertEquals(addUpTo(99), 4950);
  });
});

