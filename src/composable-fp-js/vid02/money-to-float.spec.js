// import { moneyToFloat } from './money-to-float-v1.js';
import { moneyToFloat } from './money-to-float-v2.js';

describe('moneyToFloat()', () => {
  [
    ['$0.00', 0], /* <1> */
    ['$1.00', 1], /* <2> */
    ['$1.10', 1.1], /* <3> */
    ['$1.99', 1.99],
    ['$3.14', 3.14],
  ].forEach(([input, output]) => {
    it(`should parse '${input}' USD to a ${output} float value`, () => {
      expect(moneyToFloat(input)).toEqual(output);
    });
  });
});
