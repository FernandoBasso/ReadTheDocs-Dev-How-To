export const MODNAME = "e14a-brand-type-util";

const log: Console["log"] = console.log.bind(console);

function brand<
  PrimitiveType,
  BrandName,
  Brand extends PrimitiveType & { __brand: BrandName },
>(
  isTypeFn: (value: PrimitiveType) => boolean,
  brandName: BrandName,
): [(value: PrimitiveType) => value is Brand, Brand] {
  function isBrand(value: PrimitiveType): value is Brand {
    return isTypeFn(value);
  }

  return [isBrand, {} as Brand];
}

const [isValidEmail, ValidEmailObj] = brand(
  (email: string) => email.includes("@"),
  "ValidEmail" as const,
);

type ValidEmail = typeof ValidEmailObj;

const email: string = "me@example.com";

if (isValidEmail(email)) {
  //
  // Inside the if clause block, the email is indeed valid.
  // 
  const valid: ValidEmail = email;
  log("Valid:", email);
}
else {
  //
  // But it is certainly not valid here and thus the assignment
  // to a variable of type ValidEmail does not type-check.
  //
  const oops: ValidEmail = email;
  log("Invalid:", email);
}


const [isPositive, PositiveObj] = brand(
  (x: number) => x > 0,
  "PositiveNumber" as const,
);

type PositiveNumber = typeof PositiveObj;

var x = -1;

if (isPositive(x))
  log("Positive:", x);
else
  log("Not positive:", x);

//
// And all the methods for the underlying primitive are still
// part of the intellisense.
//
