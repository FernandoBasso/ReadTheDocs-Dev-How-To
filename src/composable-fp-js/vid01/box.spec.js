// import { nextCharFromNumStr } from './box-v1';
// import { nextCharFromNumStr } from './box-v2';
// import { nextCharFromNumStr } from './box-v3';
// import { nextCharFromNumStr } from './box-v4';
import { nextCharFromNumStr } from './box-v5';

describe('nextCharFromNumStr()', () => {
  [
    ['64 ', 'A'],
    [' 89 ', 'Z'],
    ['   96', 'a'],
    ['  121 ', 'z'],
  ].forEach(([input, expected]) => {
    it(`should convert '${input}' to '${expected}'`, () => {
      expect(nextCharFromNumStr(input)).toEqual(expected);
    });
  });
});
