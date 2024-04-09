export {};

//
// Not from the video, but based on branded-type example
// from the video.
//

const log: Console["log"] = console.log.bind(console);

type PositiveNumber = number & { __brand: "PositiveNumber" };

function isPositive(x: number): x is PositiveNumber {
  return x >= 0;
}

(function (n: number): void {
  if (isPositive(n))
    // Here y is PositiveNumber.
    log("N is positive:", n);
  else
    // Here y is number.
    log("n is negative:", n);
})(-1);

(function (n: number): void {
  if (isPositive(n))
    // Here y is PositiveNumber.
    log("N is positive:", n);
  else
    // Here y is number.
    log("n is negative:", n);
})(1);
