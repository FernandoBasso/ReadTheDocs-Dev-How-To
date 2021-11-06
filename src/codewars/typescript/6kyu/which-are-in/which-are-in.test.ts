import { assertEquals } from '/deps.ts';
import { inArray } from './which-are-in-v1.ts';

Deno.test('inArray()', async (t) => {
  const haystack = ['lively', 'alive', 'harp', 'sharp', 'armstrong'];

  await t.step('should find no substrings', () => {
    assertEquals(inArray(['tarp', 'mice', 'bull'], haystack), []);
  });

  await t.step('should find two substrings', () => {
    assertEquals(
      inArray(['xyz', 'live', 'strong'], haystack),
      ['live', 'strong'],
    );
  });

  await t.step('should find all three substrings', () => {
    assertEquals(
      inArray(['live', 'strong', 'arp'], haystack),
      ['arp', 'live', 'strong'],
    );
  });
});
