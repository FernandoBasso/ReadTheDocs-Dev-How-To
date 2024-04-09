import { assertEquals } from "/deps.ts";
import { countAlphaNum } from "./countAlphaNum-v3.ts";

Deno.test("countAlphaNum()", async (t) => {
  await t.step("should return an empty object for empty string input", () => {
    assertEquals(countAlphaNum(""), {});
    assertEquals(countAlphaNum(new String("") as string), {});
  });

  await t.step("should work with simple lowercase char strings", () => {
    assertEquals(countAlphaNum("zz"), { z: 2 });
    assertEquals(countAlphaNum("xyzxyx"), { x: 3, y: 2, z: 1 });
  });

  await t.step(
    "should work with strings containing lowercase and uppercase chars",
    () => {
      assertEquals(countAlphaNum("zzZ"), { z: 3 });
      assertEquals(countAlphaNum("xYzXyx"), { x: 3, y: 2, z: 1 });
    },
  );

  await t.step("should work with alphanumeric strings", () => {
    assertEquals(
      countAlphaNum("xYzX7Z57z"),
      { x: 2, y: 1, z: 3, 7: 2, 5: 1 },
    );
  });

  await t.step("should work with any complex string", () => {
    assertEquals(
      countAlphaNum("404 Errors!"),
      { 4: 2, 0: 1, e: 1, r: 3, o: 1, s: 1 },
    );

    assertEquals(
      countAlphaNum("Execute order 66!"),
      {
        e: 4,
        x: 1,
        c: 1,
        u: 1,
        t: 1,
        o: 1,
        r: 2,
        d: 1,
        6: 2,
      },
    );
  });
});
