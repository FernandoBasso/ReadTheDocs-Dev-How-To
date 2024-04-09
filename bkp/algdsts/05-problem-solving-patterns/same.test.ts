import { assertEquals } from "/deps.ts";
import { same } from './same-v2.ts';

Deno.test('same() ', async (t) => {
  await t.step('when any value in a1 lack a matching value in a2', async (t) => {
    await t.step('should return false', () => {
      // 1 squared is 1, but it is not in the second array.
      assertEquals(same([1, 2, 3], [4, 9]), false);

      // Just because 4 is in the second array, it doesn't mean the
      // results have the same frequency. 4 should appear twice in the
      // second array because 2 appears twice in the first array. 9
      // should appear once in the second array.
      assertEquals(same([2, 3, 2], [4, 9, 9]), false);
    });
  });

  await t.step('when all values in a1 have matching values in a2', async (t) => {
    await t.step('should return true', () => {
      assertEquals(same([1, 2, 3], [1, 4, 9]), true);
      assertEquals(same([1, 2, 3], [9, 1, 4]), true);
    });
  });
});
