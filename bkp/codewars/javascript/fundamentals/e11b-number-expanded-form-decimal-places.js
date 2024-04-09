/*
--------------------------------------------------------------------------------
= Write Number in Expanded Form - Part 2

https://www.codewars.com/kata/write-number-in-expanded-form-part-2

--------------------------------------------------------------------------------
*/

import expect from 'expect';

/* eslint-disable no-unused-vars */
import { l, allTestsPassed } from '../lib';

//
// expandedForm :: number -> string
// Produce expanded form of a number with integral and fractional parts.
//
// ASSUME: `num` contains a '.' separator.
//
function expandedForm(num) {
  const [integral, decimal] = String(num).split('.');

  return [...integral].reverse().map((e, i) => e * 10 ** i).reverse()
    .concat([...decimal].map((e, i) => `${e}/${10 ** i * 10}`))
    .filter((e) => !String(e).startsWith('0'))
    .join(' + ');
}

expect(expandedForm(0.04)).toEqual('4/100');
expect(expandedForm(0.040)).toEqual('4/100');
expect(expandedForm(1.24)).toEqual('1 + 2/10 + 4/100');
expect(expandedForm(7.304)).toEqual('7 + 3/10 + 4/1000');
expect(expandedForm(54.321)).toEqual('50 + 4 + 3/10 + 2/100 + 1/1000');
expect(
  expandedForm(98076.54321),
).toEqual('90000 + 8000 + 70 + 6 + 5/10 + 4/100 + 3/1000 + 2/10000 + 1/100000');

allTestsPassed('All Tests Passed!');

/*
--------------------------------------------------------------------------------
== Explanation

Just one function. Confusing doing everything in one lengthy go. Just to show
a different approach, though.

--------------------------------------------------------------------------------
*/

