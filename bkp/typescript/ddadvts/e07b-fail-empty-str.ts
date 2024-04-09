export const MODNAME = "e07b-fail-empty-str";

////
// We can do T extends "" because TS has type literals!
//
type NoEmptyStr<T extends string> = T extends "" ? never : T;

function failOnEmptyStr<T extends string>(val: NoEmptyStr<T>) {
  if (val.length < 1)
    throw new Error("Input string is empty.");
}

////
// Now the empty string will be checked. Because we passed an
// empty string, it will cause the conditional type we used
// for the param to result in never, and nothing can be
// assigned to `never`.
//
failOnEmptyStr("");
// ~ Argument of type 'string' is not assignable to parameter
// ~ of type 'never'.
//

failOnEmptyStr("Hello!");
