export const MODNAME = "e07a-inpfail-empty-str";

function failOnEmptyStr(val: string): true | never {
  if (val.length < 1)
    throw new Error("Input string is empty.");

  return !!1;
}

//
// It will throw an exception at runtime, but our types are
// not catching the fact that we are passing an empty string.
//
failOnEmptyStr("");
