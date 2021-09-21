import { Box } from './box';

describe('Box()', () => {
  [
    [1, 'Box(1)'],
    ['FP', 'Box(FP)'],
  ].forEach(([input, output]) => {
    it(`should toString ${input}`, () => {
      expect(String(Box(input))).toEqual(output);
    });
  });

  //
  // Not sure how to unit test map without any help from the
  // stringification of the object, since we have to assert that
  // whatever function pass given map(), it does apply to the value and
  // return the box with the value modified by that function.
  //
  // We need fold to unbox the value.
  //
  describe('Box.map()', () => {
    [
      ['id', 'Tomb Raider I 1996', v => v, 'Tomb Raider I 1996'],
      ['toLower', 'JEDI', s => s.toLowerCase(), 'jedi'],
      ['sub1', 0, i => i - 1, 0 - 1],
    ].forEach(([name, input, fn, output]) => {
      it(`should apply ${name} correctly`, () => {
        expect(String(Box(input).map(fn))).toEqual(String(Box(output)));
        expect(Box(input).map(fn).toString()).toEqual(Box(output).toString());
      });
    });
  });

  describe('Box.fold()', () => {
    [
      ['id', 1, v => v, 1],
      ['add1', 0, v => v + 1, 0 + 1],
      ['split and join', 'xyz', v => v.split('').join('-'), 'x-y-z'],
      ['Number', '3.14', Number, 3.14],
    ].forEach(([name, input, fn, output]) => {
      it(`should apply ${name} to ${input} and produce ${output}`, () => {
        expect(Box(input).fold(fn)).toEqual(output);
      });
    });
  });

  describe('should have chainable map with fold unboxing the value', () => {
    it('should toLower, split, join and then unbox', () => {
      const toLower = s => s.toLowerCase();
      const split = (sep, val) => val.split(sep);
      const join = (sep, val) => val.join(sep);

      const strToArr = split.bind(null, '');
      const joinWithUnderscore = join.bind(null, '_');

      expect(
        Box('JEDI').map(toLower).map(strToArr).fold(joinWithUnderscore)
      ).toEqual('j_e_d_i');

      expect(
        Box('YODA')
          .map(s => s.toLowerCase())
          .map(s => s.split(''))
          .fold(s => s.join('-'))
      ).toEqual('y-o-d-a');
    });
  });
});
