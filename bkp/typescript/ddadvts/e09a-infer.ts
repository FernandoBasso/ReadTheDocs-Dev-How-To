export const MODNAME = "e09a-infer";

function add(x: number, y: number): number {
  return x + y;
}

////
// NOTE: TypeScript already provides the utility type
// `ReturnType`.
//
type RetType<T> =
  T extends (...args: any[]) => infer R
    ? R
    : unknown;
//
// Attempt to match anything that looks like a function and
// infer (capture) the type of its return in `R`. If it does
// look like a function, then the ternary will return `R`,
// otherwise, `unknown`.
//

type AddReturn = ReturnType<typeof add>;
// type AddReturn = number
