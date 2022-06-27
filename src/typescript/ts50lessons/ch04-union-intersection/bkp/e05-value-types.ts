export const NAME = "e05 value types a.k.a. literal types";

const log: Console["log"] = console.log.bind(console);

//
// Value types are also known as literal types.
//

let withAnyType: any = "foo";
let withUnknownType: unknown = "foo";
let withStringType: string = "foo";

//
// The type is the literal string "foo".
//
let withValueAkaLiteralType: "foo" = "foo";

//
// Can't be "bar". Has to be "foo".
//
withValueAkaLiteralType = "bar";


var a = "may";
// ‘a’ is inferred as string, and it can
// be reassigned to something else.

let b = "the";
// ‘b’ is inferred as string and it can be
// reassigned to something else.

const c = "force";
// ‘c’ is inferred as the value type (literal type)
// "force" because it can't possibly be reassigned.

