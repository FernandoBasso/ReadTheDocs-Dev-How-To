import { id } from './id';

describe('returns its input', () => {
  [
    [1, '1'],
    [null, 'null'],
    [undefined, 'undefined'],
    ['', "'1'"],
    [{ foo: 'bar' }, "{ foo: 'bar' }"],
    [[10, 20], '[10, 20]'],
  ].forEach(([value, description]) => {
    it(`should output its input ${description}`, () => {
      expect(id(value)).toEqual(value);
    });
  });
});
