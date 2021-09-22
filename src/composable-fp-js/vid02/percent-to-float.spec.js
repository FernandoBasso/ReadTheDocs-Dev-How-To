// import { percentToFloat } from './percent-to-float-v1.js';
import { percentToFloat } from './percent-to-float-v2.js';

describe('percentToFloat()', () => {
  [
    ['20%', 0.2],
    ['5%', 0.05],
    ['50%', 0.5],
    ['100%', 1],
  ].forEach(([input, output]) => {
    it(`should parse '${input}' USD to a ${output} float value`, () => {
      expect(percentToFloat(input)).toEqual(output);
    });
  });
});
