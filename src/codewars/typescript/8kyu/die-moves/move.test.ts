import { assertEquals } from "/deps.ts";
import { move } from "./move_v1.ts";

Deno.test("move()", async (t) => {
  await t.step("move(0, 4)", () => {
    assertEquals(move(0, 4), 0 + 4 * 2); // 8
  });

  await t.step("move(3, 6)", () => {
    assertEquals(move(3, 6), 3 + 6 * 2); // 15
  });

  await t.step("move(2, 5)", () => {
    assertEquals(move(2, 5), 2 + 5 * 2); // 12
  });
});
