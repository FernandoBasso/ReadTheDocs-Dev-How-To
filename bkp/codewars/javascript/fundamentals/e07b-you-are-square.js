/*

https://www.codewars.com/kata/youre-a-square/train/javascript

ASSUME: input is an integral number.

If `x * x = n`, then `n` is a perfect square.

*/


const l = console.log.bind(console);

// isSquare :: number -> boolean
// Produce true if n is a perfect square; false otherwise.
function isSquare(n) {
  // return Math.sqrt(n) % 1 === 0;
  return Number.isInteger(Math.sqrt(n));
}

/*
This is another possible implementation.

    function isSquare(n) {
      const half = n / 2;

      for (let i = 0; i < half; ++i)
        if (i * i === n) return true;

      return false;
    };
*/


l('-1:', isSquare(-1));
// → false, Negative numbers cannot be square numbers.

l(' 0:', isSquare(0));
//, true, 0 is a square number: 0 * 0.

l(' 3:', isSquare(3));
// → false, 3 is not a square number.

l(' 4:', isSquare(4));
// → true, 4 is a square number: 2 * 2.

l(' 9:', isSquare(9));
// → true, 9 is a square number: 3 * 3.

l('25:', isSquare(25));
// → true, 25 is a square number: 5 * 5.

l('26:', isSquare(26));
// → false, 26 is not a square number.


/*

https://www.quora.com/What-is-the-quickest-way-to-determine-if-a-number-is-a-perfect-square

Mind-blowing stuff:
https://math.stackexchange.com/questions/41337/efficient-way-to-determine-if-a-number-is-perfect-square
 
*/
