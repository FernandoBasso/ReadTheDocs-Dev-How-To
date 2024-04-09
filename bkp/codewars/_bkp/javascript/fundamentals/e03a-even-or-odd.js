//
// https://www.codewars.com/kata/53da3dbb4a5168369a0000fe/solutions/javascript
//

const l = console.log.bind(console);

function evenOrOdd(n) {
  return n % 2 === 0 ? 'Even' : 'Odd';
} 

l(evenOrOdd(3));
// → Odd

l(evenOrOdd(4));
// → Even

