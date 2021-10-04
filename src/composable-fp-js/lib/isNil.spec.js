import { isNil } from './isNil';

describe('isNil()', () => {
  //
  // We say “empty array/object” with a loose interpretation of “empty",
  // because we know they inherit some methods and properties. We mean
  // “empty” in the sense that we didn't explicitly add values to them
  // ourselves.
  //
  [
    [undefined, 'undefined', true],
    [null, 'null', true],
    ['', 'empty string', false],
    [0, '0 (zero)', false],
    [[], '[] (empty array)', false],
    [{}, '{} (empty object)', false],
    [NaN, 'NaN', false],
  ].forEach(function checkIsNil([input, description, expected]) {
    it(`should return ${expected} for ${description}`, () => {
      expect(isNil(input)).toEqual(expected);
    });
  });
});
