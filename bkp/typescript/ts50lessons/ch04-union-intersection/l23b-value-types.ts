export const NAME = "l23a Value Types (a.k.a. Literal Types";

//
// Each value of a specific set is its own type. It is then
// called a value type (or literal type).
//
// Using ‘var’ and ‘let’ mean we could reassign and the value
// could change, but not with ‘const’ (for primitives).
//
var s1 = "hello";
// → string

let s2 = "world";
// → string

const s3 = "magnificent";
// → "magnificent".
//
// Yes, the type is the literal string "magnificent". Because
// we used ‘const’, the only possible value for ‘s3’ is the
// string "magnificent", therefore, this specific literal
// value is the type.
//
// Hover over the identifiers and see for yourself.
//
//
// We can do a type annotation as well:
//
var one: 1 = 1;
let two: "two" = "two";

//
// Or use the ‘as const’ type assertion!
//
var three = "III" as const;
