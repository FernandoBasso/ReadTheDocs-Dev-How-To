import { assertEquals } from "/deps.ts";

Deno.test("countBits()", async (t) => {
  await t.step("should produce 0 for input 0", () => {
      assertEquals(countBits(0), 0);
  });

      // assert.equal(countBits(4), 1);
      // assert.equal(countBits(7), 3);
      // assert.equal(countBits(9), 2);
      // assert.equal(countBits(10), 2);
});

