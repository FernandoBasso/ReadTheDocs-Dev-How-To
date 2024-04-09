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
function expandedForm(num) {
  return [...String(num)].reverse()
    .map((elem, index) => elem * 10 ** index)
    .filter((elem) => elem)
    .reverse()
    .join(' + ');
}

expect(expandedForm(3)).toEqual('3');
expect(expandedForm(34)).toEqual('30 + 4');
expect(expandedForm(345)).toEqual('300 + 40 + 5');
expect(expandedForm(3456)).toEqual('3000 + 400 + 50 + 6');
expect(expandedForm(34567)).toEqual('30000 + 4000 + 500 + 60 + 7');

// Tests with zeroes are _very_ important.
expect(expandedForm(70304)).toEqual('70000 + 300 + 4');

// Ends with zero. Important to test too.
expect(expandedForm(70340)).toEqual('70000 + 300 + 40');
expect(expandedForm(70300)).toEqual('70000 + 300');


allTestsPassed('All Tests Passed!');

/*
--------------------------------------------------------------------------------
== Explanation

By reversing the digits of the number we can apply a power of 10 to get it
added the correct number of zeroes based on the digits position and making
use of the index parameter map's callback function accepts.

    5043
    3405

    3 * 10 ** 0 = 3e0 = 3
    4 * 10 ** 1 = 4e1 = 40
    0 * 10 ** 2 = 0e2 = 0
    5 * 10 ** 3 = 5e3 = 5000

So, we split the number into an array of its digits, reverse the array, map
to apply the power of 10 times the digits's index in the array, reverse it
back, and join the elements with ' + ' to produce the resulting string.

Even though the splitting creates an array of strings ['5', '4', '3'], for
example, the `*` and `**` operators implicitly converts them to numbers, so,
no explicit conversion is needed.


    ['3', '4', '0', '5'].forEach((e, i) => {
      log(e, i, e * 10 ** i);
    });
    // → 3 0 3
    // → 4 1 40
    // → 0 2 0
    // → 5 3 5000

--------------------------------------------------------------------------------
*/

// vim: set textwidth=78:

