//
// https://www.codewars.com/kata/546f922b54af40e1e90001da/train/javascript
//

const l = console.log.bind(console);

/**
 * Replace letter with its position in the alphabet.
 * 
 * @param {string} s
 * @return {string}
 */
function alphabetPosition(s) {
  const aCharCode = 'a'.charCodeAt(); // 97

  ////
  // Produces an array with the position of each char.
  return s.toLowerCase()
    .match(/[a-z]/g)
    .map(n => n.charCodeAt() - 97 + 1)
    .join(' ');
}

l(alphabetPosition('aBc XyZ'));
// → 1 2 3 24 25 26

l(alphabetPosition('The sunset sets at twelve o\' clock.'));
// → "20 8 5 19 21 14 19 5 20 19 5 20 19 1 20 20 23 5 12 22 5 15 3 12 15 3 11")

l(alphabetPosition('The narwhal bacons at midnight.'));
// →  "20 8 5 14 1 18 23 8 1 12 2 1 3 15 14 19 1 20 13 9 4 14 9 7 8 20")

/*

== Explanation
--------------

`a' has char code 97.

If we get the charcode of any letter in the alphabet, we can do:

    position_in_the_alphabet = letterCharCode - charCodeOfa

    a = 97
    b = 98
    a - a = 0
    a - b = 1

- `a` is at position 0
- `b` is at position 1


And we use `+ 1` to make `a` be at position 1, `b` at position 2, and so forth.

The technique of using the decimal value of a character is demostrated
in the K&R C book.


=== `filter' vs `match'
-----------------------

Instead of

    match(/[a-z]/g)

we could also have used

     .split('')
     .filter(e => /[a-z]/.test(e))

But then we would need both `filter` and `split`. `match' returns an array
when the `g` flag is used, and therefore, no `split` is needed for this case.

 */