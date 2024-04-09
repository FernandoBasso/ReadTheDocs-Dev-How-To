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
function expandedForm(num, acc = []) {
  const len = String(num).length;

  // The base case.
  if (len === 1 && num === 0) return acc.join(' + ');
  if (len === 1) return [...acc, num].join(' + ');

  // <1>
  const bigger = num - (num % (10 ** (len - 1)));

  // <2>
  const remainder = num - bigger;

  return expandedForm(remainder, [...acc, bigger]);
}

expect(expandedForm(3)).toEqual('3');
expect(expandedForm(34)).toEqual('30 + 4');
expect(expandedForm(345)).toEqual('300 + 40 + 5');
expect(expandedForm(3456)).toEqual('3000 + 400 + 50 + 6');
expect(expandedForm(34567)).toEqual('30000 + 4000 + 500 + 60 + 7');

// Tests with zeroes are _very_ important.
expect(expandedForm(70304)).toEqual('70000 + 300 + 4');


allTestsPassed('All Tests Passed!');

/*
--------------------------------------------------------------------------------
== Explanation

<1>: Uses some math magic to get the 10, 100, 1000, etc. part of the number.

    34 - 34 % 10 ** 1
    // → 30
    345 - 345 % 10 ** 2
    // →  300
    3456 - 3456 % 10 ** 3
    // → 3000

Note that 1, 2 and 3 at the end of the expressions is the length of digits
minus 1. So, 345 has 3 digits, minus 1, equals 2.

    34, get 30
    345, get 300
    3456, get 3000

<2>: Uses a similar math magic to get the remainder after the “bigger” number.
Same thing as doing:

    const remainder = num % 10 ** (len - 1);

But since we already have `bigger`, we might as well just use it for a
simpler expression.

Also, the computation of the remainder takes care of eliminating the leading
zeroes automatically (a pure math thing, we didn't do it intentionally), so, we
never have to worry about inadvertently placing a '+ 0' into the result.

--------------------------------------------------------------------------------
*/

