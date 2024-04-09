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
// isZero :: number => boolean
// Produce true if x is zero; false otherwise.
//
const isZero = (x) => x === 0;

//
// isNotZero :: number => boolean
// Produce true if x is not zero; false otherwise.
//
const isNotZero = (x) => !isZero(x);

//
// expandIntegral :: number -> string
//                :: string -> string
// Produce expanded form of the an integral number.
//
// Example input/output:
//
//    3045
//    ['3000', '40', '5']
//
function expandIntegral(num) {
  return [...String(num)].reverse()
    .map((elem, index) => elem * 10 ** index)
    .filter((elem) => elem)
    .reverse()
    .filter(isNotZero);
}

//
// expandedForm :: number -> string
//              :: string -> string
// Produce expanded form of the decimal part of a number.
//
// Example input/output:
//
//    3045
//    ['3/10', '4/1000', '5/10000']
//
function expandDecimal(num) {
  return [...String(num)].reduce((acc, e, i) => {
    // If number is zero, ignore it.
    if (Number(e) === 0) return acc;

    // Compute the x/y fractional part.
    //              <1>
    return [...acc, `${e}/${10 ** i * 10}`];
  }, [])
    .filter(isNotZero);
}

//
// expandedForm :: number -> string
// Produce expanded form of a number with integral and fractional parts.
//
// ASSUME: `num` contains a '.' separator.
//
function expandedForm(num) {
  const [integral, decimal] = String(num).split('.');

  return [
    ...expandIntegral(integral),
    ...expandDecimal(decimal),
  ].join(' + ');
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

For `expandDecimal`, consider:

    10 ** 0 * 10
    // → 10
    10 ** 1 * 10
    // → 100
    10 ** 2 * 10
    // → 1000
    10 ** 3 * 10
    // → 10000

So, for each digit, we return the digit, concatenated with a '/', concatenated
by the result of `10 ** i * 10`, which will produce, 10, 100, 1000, etc. as
the index parameter of the callback function increases.

Other than that, we just make sure we don't returns zeroes, and then join the
elements of the array with `' + '` to produce the final result.

Although we are saying our `epxandIntegral` and `expandDecimal` take a
number, we split the number on the decimal separator and end up with two
(numeric) strings, not two numbers.

    String(5.3).split('.')

The snippet above produces an array of strings ['5', '3'], not an array of
numbers `[5, 3]`. Still, our `expandIntegral` and `expandDecimal` functions
can work with both numbers and strings. Thus, the multiple signatures. Read
more about this here:

https://github.com/ramda/ramda/wiki/Type-Signatures#multiple-signatures

--------------------------------------------------------------------------------
*/
