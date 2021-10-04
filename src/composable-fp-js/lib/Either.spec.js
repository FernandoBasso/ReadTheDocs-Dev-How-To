import { Right, Left } from './Either';

describe('Left()', () => {
  describe('Left().toString()', () => {
    [
      [1, 'Left(1)'],
      ['FP', 'Left(FP)'],
    ].forEach(([input, output]) => {
      it(`should toString ${input}`, () => {
        expect(String(Left(input))).toEqual(output);
      });
    });
  });

  describe('Left().map()', () => {
    [
      ['id', 'Tomb Raider I 1996', v => v, 'Tomb Raider I 1996'],
      ['toLower', 'JEDI', s => s.toLowerCase(), 'JEDI'],
      ['sub1', 0, i => i - 1, 0],
    ].forEach(([name, input, fn, output]) => {
      it(`should NOT apply ${name}`, () => {
        expect(
          String(Left(input).map(fn))
        ).toEqual(String(Left(output)));
      });
    });
  });

  describe('Left().fold()', () => {
    it('should return the unprocessed value', () => {
      expect(
        Left('jedi')
          .fold(
            _ => 'Error: no processing performed',
            str => str.toUpperCase()
          )
      ).toEqual('Error: no processing performed');
    });
  });

  describe('Left().map().fold()', () => {
    it('should return the unprocessed value', () => {
      expect(
        Left('jedi')
          .map(str => str.toUpperCase())
          .map(str => str.split(''))
          .map(arr => arr.join('-'))
          .fold(
            _ => 'Error: no processing performed',
            str => str.toUpperCase()
          )
      ).toEqual('Error: no processing performed');
    });
  });
});

describe('Right()', () => {
  describe('Right().toString()', () => {
    [
      [1, 'Right(1)'],
      ['FP', 'Right(FP)'],
    ].forEach(([input, output]) => {
      it(`should toString ${input}`, () => {
        expect(String(Right(input))).toEqual(output);
      });
    });
  });

  describe('Right().map()', () => {
    [
      ['id', 'Tomb Raider I 1996', v => v, 'Tomb Raider I 1996'],
      ['toLower', 'JEDI', s => s.toLowerCase(), 'jedi'],
      ['sub1', 0, i => i - 1, 0 - 1],
    ].forEach(([name, input, fn, output]) => {
      it(`should apply ${name} correctly`, () => {
        expect(
          String(Right(input).map(fn))
        ).toEqual(String(Right(output)));

        expect(
          Right(input).map(fn).toString()
        ).toEqual(Right(output).toString());
      });
    });
  });

  describe('Right().fold()', () => {
    [
      ['id', 1, v => v, 1],
      ['add1', 0, v => v + 1, 0 + 1],
      ['split and join', 'xyz', v => v.split('').join('-'), 'x-y-z'],
      ['Number', '3.14', Number, 3.14],
    ].forEach(([name, input, fn, output]) => {
      it(`should apply ${name} to ${input} and produce ${output}`, () => {
        expect(Right(input).fold(_ => 'Error', fn)).toEqual(output);
      });
    });
  });

  describe('Right().map().fold()', () => {
    it('should return the processed value', () => {
      expect(
        Right('jedi')
          .map(str => str.toUpperCase())
          .map(str => str.split(''))
          .fold(_ => 'Error', arr => arr.join('-'))
      ).toEqual('J-E-D-I');
    });
  });

  describe('Right().map().fold()', () => {
    it('should return the processed value', () => {
      expect(
        Right('jedi')
          .map(str => str.toUpperCase())
          .map(str => str.split(''))
          .map(arr => arr.join('-'))
          .fold(_ => 'Error', v => v)
      ).toEqual('J-E-D-I');
    });

    it('should toLower, split, join and then unbox', () => {
      const toLower = s => s.toLowerCase();
      const split = (sep, val) => val.split(sep);
      const join = (sep, val) => val.join(sep);

      const strToArr = split.bind(null, '');
      const joinWithUnderscore = join.bind(null, '_');

      const f = () => undefined;

      expect(
        Right('JEDI').map(toLower).map(strToArr).fold(f, joinWithUnderscore)
      ).toEqual('j_e_d_i');

      expect(
        Right('YODA')
          .map(s => s.toLowerCase())
          .map(s => s.split(''))
          .fold(_ => undefined, s => s.join('-'))
      ).toEqual('y-o-d-a');
    });
  });
});

