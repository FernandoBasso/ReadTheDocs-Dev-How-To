//
// https://www.codewars.com/kata/53da3dbb4a5168369a0000fe/solutions/javascript
//

const l = console.log.bind(console);

function evenOrOdd(n) {
  return ['Even', 'Odd'][n & 1];
} 

l(evenOrOdd(3));
// → Odd

l(evenOrOdd(4));
// → Even

/*

Here we make use of the `&` (AND) bitewise operator. For any expression

    number & 1

the result will always be 0 if `number` is even, and 1 if it is `odd`.
Since it will be either zero or one, we have a means to get the indexes
0 or 1 of an array.

----
    0 b 1 0 1 0 = 10
  & 0 b 0 0 0 1 = 1
    -----------
    0 b 0 0 0 0 = 0, 10 is even


    0 b 1 0 1 = 5
  & 0 b 0 0 1 = 1
    ---------
    0 b 0 0 1 = 1, 5 is odd
----


*/