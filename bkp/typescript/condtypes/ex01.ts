export const NAME = "ex01 on conditional types";

function f<T extends boolean>(a: T): T extends true ? string : number;
function f(a: boolean): number | string {
  return (a ? "1" : 1);
}

var x = f(true);

var y = f(false);
