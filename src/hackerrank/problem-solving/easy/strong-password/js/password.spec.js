import { minNum } from './password-v4.js';

describe('minNum(n, s)', () => {
  test('empty input', () => {
    expect(minNum(0, '')).toEqual(6);
  });

  test('missing 1 char', () => {
    // missing a digit
    expect(minNum(5, 'llUU%')).toEqual(1);

    // still missing a digit
    expect(minNum(6, 'llUU%%')).toEqual(1);

    // missing a special char
    expect(minNum(5, 'llUU9')).toEqual(1);

    // still missing a special char
    expect(minNum(6, 'llUU99')).toEqual(1);
  });

  test('missing 5 char', () => {
    expect(minNum(1, 'z')).toEqual(5);
    expect(minNum(1, 'Z')).toEqual(5);
    expect(minNum(1, '9')).toEqual(5);
    expect(minNum(1, '%')).toEqual(5);
    expect(minNum(6, 'zzzzzz')).toEqual(3);
  });

  test('when nothing else is missing', () => {
    expect(minNum(6, 's3cR%t')).toEqual(0);
  });

  test('when length is >= 6 and missing 1', () => {
    expect(minNum(7, 'lUUU999')).toEqual(1);
  });
});
