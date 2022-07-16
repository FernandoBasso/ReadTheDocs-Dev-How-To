import { assertEquals } from "/deps.ts";
import { add } from "./add.ts";

Deno.test("add()", async (t) => {
  await t.step("should add any two numbers", () => {
    assertEquals(add(0, 0), 0);
    assertEquals(add(0, -0), 0);
    assertEquals(add(1.7, -4), -2.3);
    assertEquals(add(-1e3, -1), -1001);
  });
});
