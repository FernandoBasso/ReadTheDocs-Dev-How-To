export const MODNAME = "non-negative";

type NonZero<Num extends number> = Num extends 0 ? never : Num;

type Z = NonZero<0>;
type N = NonZero<-1>;

function failOnZero<T extends number>(num: NonZero<T>): number | never {
  if (num === 0)
    throw new RangeError("Input number must be non-zero.");

  return num;
}

const x = failOnZero(1);
// x: number

const y = failOnZero(0);
// y: number
// Why `number`? Why not `never`?
