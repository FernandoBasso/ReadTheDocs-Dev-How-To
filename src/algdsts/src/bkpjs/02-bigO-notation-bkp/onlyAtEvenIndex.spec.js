import { onlyAtEvenIndex } from './onlyAtEvenIndex-v2';

describe('onlyAtEvenIndex()', () => {
  it('should return only the elements at even indexes', () => {
    [
      [
        ['may', 'the', 'force'],
        ['may', 'force'],
      ],
      [
        ['the', 'force', 'is', 'strong', 'with', 'this', 'one'],
        ['the', 'is', 'with', 'one'],
      ],
      [
        [0, 1, 2, 3, 4],
        [0, 2, 4],
      ],
      [
        [undefined, 1, null, 3, NaN],
        [undefined, null, NaN],
      ],
      [
        [0, undefined, 2, null, 4, NaN],
        [0, 2, 4],
      ],
    ].forEach(([input, output]) => {
      expect(onlyAtEvenIndex(input)).toEqual(output);
    });
  });
});
