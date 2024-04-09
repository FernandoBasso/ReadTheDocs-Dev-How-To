/*

https://www.codewars.com/kata/mumbling/train/javascript

*/

const expect = require('expect');
const l = console.log.bind(console);

// accum :: string -> string
// Produces a “mumblified” version of a string.
function accum(s) {
  return [...s].reduce((acc, chr, idx) => {
    return [
      ...acc,
      chr.toUpperCase() + chr.toLowerCase().repeat(idx),
    ];
  }, []).join('-');
}

expect(accum('a')).toEqual('A');
expect(accum('ab')).toEqual('A-Bb');
expect(accum('abc')).toEqual('A-Bb-Ccc');

expect(accum('abcd')).toEqual('A-Bb-Ccc-Dddd');
expect(accum('RqaEzty')).toEqual('R-Qq-Aaa-Eeee-Zzzzz-Tttttt-Yyyyyyy');
expect(accum('cwAt')).toEqual('C-Ww-Aaa-Tttt');


l('\nAll tests passed!');

/*

`[...s]` produces an array of characters, in the same way that `s.split('')
would do.

Then, we reduce the array to create an array with the uppercased first letter concatenated with the lowercased, repeated letters.

Also, `chr.toLowerCase().repeat(idx)` is better than `chr.repeat(idx).toLowerCase()`
because with the former we uppercase only one char, and then repeat, rather than
repeating and then lowercasing several chars.

We end with an array like `['A', 'Bb', 'Ccc']` which we then `join('-')` to
create the final, resulting string.

*/