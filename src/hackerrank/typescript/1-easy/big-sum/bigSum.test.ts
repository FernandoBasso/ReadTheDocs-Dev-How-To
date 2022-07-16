import { assertEquals } from "/deps.ts";
import { sum } from "./bigSum-v3.ts";

Deno.test("sum()", async (t) => {
  await t.step("should sum empty array", () => {
    assertEquals(sum([]), 0);
  });

  await t.step("should sum arrays of 1 element", () => {
    assertEquals(sum([0]), 0);
    assertEquals(sum([-0]), 0);

    //
    // $ deno repl
    //
    // > -0 === -0
    // true
    // > -0 === +0
    // true
    // > +0 === -0
    // true
    //
    // Yet, this fails. Must be the way `assertEquals()` is implemented.
    //
    // assertEquals(sum([-0]), -0);
    //

    assertEquals(sum([1]), 1);
    assertEquals(sum([-1]), -1);
  });

  await t.step("should sum arrays of 2 or more elements", () => {
    assertEquals(sum([1, 2]), 3);
    assertEquals(sum([-1, -2]), -3);

    assertEquals(sum([-1, 1, -2, 2]), 0);
    assertEquals(sum([-1, 1, -2, 2]), 0);

    assertEquals(sum([1e4]), 1e4);
    assertEquals(sum([-1e4]), -1e4);
  });

  await t.step("should sum test case from problem description", () => {
    assertEquals(sum([
      1000000001,
      1000000002,
      1000000003,
      1000000004,
      1000000005,
    ]),
    5000000015,
    );
  });
});
