//
// https://www.codewars.com/kata/546f922b54af40e1e90001da/train/javascript
//

const l = console.log.bind(console);

/**
 * Creates an array with the lowercase alphabet.
 * 
 * COMPLAINT: How do I miss ranges in JavaScript like we have
 * in Ruby, Haskell and other languages. Bash's beloved {a..z}
 * 
 * @return {array<string>}
 */
function makeAlphabet() {
  const aCharCode = 'a'.charCodeAt(); // 97
  const zCharCode = 'z'.charCodeAt(); // 122

  const arr = [];
  for (let i = aCharCode; i <= zCharCode; ++i)
  arr.push(String.fromCharCode(i));

  return arr;
}

/**
 * Replace letter with its position in the alphabet.
 * 
 * @param {string} s
 * @return {string}
 */
function alphabetPosition(s) {
  const alphabet = makeAlphabet();

  ////
  // Cleans the input string for non-alphabetic, a-z chars.
  const arr = s.toLowerCase().split('').filter(e => /[a-z]/.test(e));

  ////
  // Produces an array with the position of each char.
  // and joins the  array using spaces.
  return arr.reduce((acc, e) => {
    return [...acc, alphabet.indexOf(e) + 1];
  }, []).join(' ');
}

l(alphabetPosition('aBc XyZ'));
// → 1 2 3 24 25 26

l(alphabetPosition('The sunset sets at twelve o\' clock.'));
// → "20 8 5 19 21 14 19 5 20 19 5 20 19 1 20 20 23 5 12 22 5 15 3 12 15 3 11")

l(alphabetPosition('The narwhal bacons at midnight.'));
// →  "20 8 5 14 1 18 23 8 1 12 2 1 3 15 14 19 1 20 13 9 4 14 9 7 8 20")
