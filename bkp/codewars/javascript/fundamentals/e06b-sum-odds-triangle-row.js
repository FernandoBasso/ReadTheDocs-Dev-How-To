//
// https://www.codewars.com/kata/sum-of-odd-numbers/train/javascript
//

const l = console.log.bind(console);

//
//              1
//           3     5                    8
//        7     9    11                 27
//    13    15    17    19              64
// 21    23    25    27    29          125
//

/**
 * Sum odd numbers starting at row `n`
 */
function sumOdd(n) {
  ////
  // The count of the numbers in the row, cubed.
  return Math.pow(n, 3);
  // NOTE: Newer js engines allow `n ** 3` too.
}

l(sumOdd(1));
// → 1

l(sumOdd(2));
// → 8, 3 + 5

l(sumOdd(3));
// → 27, 7 + 9 + 11

l(sumOdd(4));
// → 64, 7 + 9 + 11

l(sumOdd(5));
// → 125, 21 + 23 + 24 + 25 + 27 + 29

l(sumOdd(42));
// → 74088

/*

If we recognize the pattern that the sum of the elements in a given row is
the same as the count of the number of elements in that row, cubed, then
the code (and math) solution is suprisingly simple.

--------------------------------------------------------------------------------

Here's another way to think about it.

You can rearrange the numbers in each row by subtracting from some terms and
adding onto others. For example, in row 4, you can take 1 away from 17 and 3
away from 19, and add them onto 15 resp. 13 to make all of the numbers equal
to 16, which is the average of row 4; not incidentally equal to 4 ^ 2.

In row 4:

    13 + 15 + 17 + 19 = 64
    32 / 2 = 16
    4 ** 2 * 4 = 64
    4 ** 3 = 64

Since there are 4 terms in row 4, the result is 4 ^ 2 * 4 = 4 ^ 3 = 4 *4*4.

    3 ** 2 * 3 = 27
    3 ** 3     = 27

    4 ** 2 * 4 = 64
    4 ** 3     = 64

    5 ** 2 * 5 = 125
    5 ** 3     = 125

    6 ** 2 * 6 = 216
    6 ** 3     = 216


    6 ** 2 * 6 === (6 * 6) * 6 === 6 ** 3

 */