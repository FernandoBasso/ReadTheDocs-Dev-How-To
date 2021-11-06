import { assertEquals } from '/deps.ts'
import { filterFriends } from './friend-foe-v1.ts';

Deno.test('filterFriends()', () => {
  [
    [[], []],
    [['K', 'Mu', 'Mia'], []],
    [['Goku', 'Lara'], ['Goku', 'Lara']],
    [['Ryan', 'Kieran', 'Mark'], ['Ryan', 'Mark']],
    [['Ryan', 'Jimmy', '123', '4', 'Cool Man'], ['Ryan']],
    [
      ['Jimm', 'Cari', 'aret', 'truehdnviegkwgvke', 'sixtyiscooooool'],
      ['Jimm', 'Cari', 'aret'],
    ],
    [['Love', 'Your', 'Face', '😄'], ['Love', 'Your', 'Face']],
  ].forEach(([input, output]) => {
    assertEquals(filterFriends(input), output);
  });
});
