import { fromNullable } from './fromNullable';
import {
  Left,
  Right,
} from './Either';

//
// Not sure about the best way to assert that we got a Left or Right,
// but it seems the toString approach is good enough for our purposes.
//

[
  [undefined, 'undefined'],
  [null, 'null'],
].forEach(([input, description]) => {
  describe(`when input is ${description}`, () => {
    it('should return a Left', () => {
      expect(
        String(fromNullable(input))
      ).toEqual(String(Left(input)));
    });
  });
});

//
// We say “empty array/object” with a loose interpretation of “empty",
// because we know they inherit some methods and properties. We mean
// “empty” in the sense that we didn't explicitly add values to them
// ourselves.
//
[
  ['', 'empty string'],
  [0, '0 (zero)'],
  [[], '[] (empty array)'],
  [{}, '{} (empty object)'],
  [NaN, 'NaN'],
].forEach(([input, description]) => {
  describe(`when input is ${description}`, () => {
    it('should return a Right', () => {
      expect(
        String(fromNullable(input))
      ).toEqual(String(Right(input)));
    });
  });
});
