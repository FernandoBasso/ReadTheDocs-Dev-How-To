export const MODNAME = "e05a-never";

////
// `never` is a type without any inhabitants. Nothing can be
// assigned to never, not even `any` can be assigned to never.
// 
const h1: never = "hello" as any;

////
// And please, NEVER (pun intended) do this.
//
const h2: never = "hello" as never;

//
// BEWARE: Don't lie about the types.
//
