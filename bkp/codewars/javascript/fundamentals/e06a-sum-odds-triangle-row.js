//
// https://www.codewars.com/kata/sum-of-odd-numbers/train/javascript
//

const l = console.log.bind(console);

//
//              1
//           3     5                    8
//        7     9    11                 27
//    13    15    17    19              64
// 21    23    25    27    29          122
//

/**
 * Sum odd numbers starting at row `n`
 */
function sumOdd(n) {
  ////
  // Finds the first odd number given the row where to start from.
  // <1>
  let first = n * (n - 1) + 1;
 
  ////
  // Applies the formula.
  // 
  // https://www.mathsisfun.com/algebra/sequences-sums-arithmetic.html
  return n / 2 * (2 * first + (n - 1) * 2) ;
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

<1>
- Row 1, first odd is 1.
- Row 2, first odd is 3.
- Row 3, first odd is 7.
- Row 4, first odd is 13.
- Row 5, first odd is 21.

In this case, the row number of the pyramid where to start is also the
number of elements in that row.

`n` is the number of elements in the sequence. Row 3 has 3 elements, row 5
has 5 elements, row 42 has 42 elements, etc.

`d` is the difference. After we start at an odd number, the next odd number
has a difference of 2 from the previous odd number.

 */