import { assertEquals } from '/deps.ts';
import { calcSubtotals } from './calcSubtotals-v1.ts';

Deno.test('calcSubtotals()', async (t) => {
	await t.step('should produce correct subtotals', () => {
		assertEquals(calcSubtotals([0, 1, 2, 3]), [
			0,
			0 + 1,
			0 + 1 + 2,
			0 + 1 + 2 + 3,
		]);
		assertEquals(calcSubtotals([10, 20, 30]), [
      10,
      10 + 20,
      10 + 20 + 30
    ]);
		assertEquals(calcSubtotals([-3, -7, -11]), [
      -3,
      -3 + -7,
      -3 + -7 + -11
    ]);
	});
});
