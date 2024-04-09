/*
--------------------------------------------------------------------------------
= Write Number In Expanded Form

https://www.codewars.com/kata/write-number-in-expanded-form/train/javascript

--------------------------------------------------------------------------------
*/

import expect from 'expect';

/* eslint-disable no-unused-vars */
import { l, allTestsPassed } from '../lib';

// expandedForm :: number -> string
// Produce expanded form of a number.
function expandedForm(n) {
  const len = String(n).length;

  return [...String(n)].reduce((acc, e, i) => {
    if (e === '0') return acc;
    return [...acc, e + '0'.repeat(len - i - 1)];
  }, []).join(' + ');
}

expect(expandedForm(3)).toEqual('3');
expect(expandedForm(34)).toEqual('30 + 4');
expect(expandedForm(345)).toEqual('300 + 40 + 5');
expect(expandedForm(3456)).toEqual('3000 + 400 + 50 + 6');
expect(expandedForm(34567)).toEqual('30000 + 4000 + 500 + 60 + 7');

// Tests with zeroes are _very_ important.
expect(expandedForm(70304)).toEqual('70000 + 300 + 4');

// Especially important with a trailing zero.
expect(expandedForm(93050)).toEqual('90000 + 3000 + 50');


allTestsPassed('All Tests Passed!');

/*
--------------------------------------------------------------------------------
== Explanation

Search the web for "mdn Array.prototype.reduce" :)

Basically, we turn our number into an array of its digits and  make use of the
reduce index which we named `i` to compute how many times we should repeat the
zero after each digit. If the number is itself zero, than we do not repeat any
zeroes.

If the number is 345, we must produce:

    300, 3 followed by two zeroes
    40, 4 followed by one zero,
    5, 5 followed by no zeroes.

`len - i - 1` is the computation to know how many times to repeat the zero.
`-1` because we need to account for the digit which is _not_ repeated.

'x'.repeat(-1)` or any other negative number raises a RangeError exception,
but it looks we never encounter such situation in the tested cases.

After the reduce we have something like `[300, 40, 5]`, which we `join(' + ')`
to produce the final result.

--------------------------------------------------------------------------------
*/

