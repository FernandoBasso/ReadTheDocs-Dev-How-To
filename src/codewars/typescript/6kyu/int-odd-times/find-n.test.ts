import { assertEquals } from '/deps.ts'
import { findN } from './find-n-v3.ts';

Deno.test('findN()', async (t) => {
  await t.step('should work for array of single number', () => {
    assertEquals(findN([0]), 0);
    assertEquals(findN([7]), 7);
    assertEquals(findN([-3]), -3);
  });

  await t.step('should work for array of three numbers', () => {
    assertEquals(findN([0, 0, 0]), 0);
    assertEquals(findN([7, 7, 7]), 7);
    assertEquals(findN([-3, -3, -3]), -3);
  });

  await t.step('should work for array of other combinations', () => {
    assertEquals(findN([0, 0, 2, 2, 0]), 0);
    assertEquals(findN([7, 4, 7, 4, 4, 4, 7]), 7);
    assertEquals(findN([-3, -7, -3, 7, -7, 7, -3]), -3);
  });
});
