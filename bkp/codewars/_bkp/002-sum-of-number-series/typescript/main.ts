//
// NOTE: The proposed function and test cases use `GetSum` with an uppercase
// “G”. Only construtor functions and components (Vue, React, etc.) should
// start with an uppercase letter.
//
const l = console.log.bind(console);

function getSum(x: number, y: number): number {
  const min: number = Math.min(x, y),
        max: number = Math.max(x, y);

  //
  // Find the number of elements in the sequence. For
  // instance, (-2, 4) = 6 elements.
  //
  const n: number = max - min + 1;

  //
  // Apply the formula:
  //
  // https://www.mathwords.com/a/arithmetic_series.htm
  //
  // Basically, we want to get the average of the first and
  // last terms multiplied by the number of terms.
  //
  return n * (max + min) / 2;
}

l(getSum(1, 5));
// → 15

l(getSum(-2, 4));
// → 7
