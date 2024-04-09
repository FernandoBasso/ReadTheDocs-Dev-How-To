export const NAME = "l43a Promisify";

const log: Console["log"] = console.log.bind(console);

//
// If we name the call back...
//
type Cb = (...args: any) => any;

//
// ...and use it here...
//
type FunctionWithCallback_1<T extends any[] = any[]> =
  (...t: [...T, Cb]) => any;
//
// ...it becomes clearer that ‘(...t: [...T], Cb])’ is a
// function whose ‘t’ param is a spread of params of type
// ‘T’, ending with a last param ‘Cb’, which we know it is
// a function intended to be used as a callback.
//
// Note we spread both the parameter `t` and the type `t`.
//

//
// Some examples of variadic tuples.
//
type T1 = [...any[], Cb];
const tup1: T1 = [1, 2, () => undefined];
const tup2: T1 = [1, 2, "Four", () => undefined];

//
// The author used this in the book, but not in the TS
// Playground. Using it in ‘promisify’ doesn't work.
//
type FunctionWithCallback<T extends any[] = any[]> =
  (...t: [...T, (...args: any) => any]) => any;

type InferArgs<T> = T extends (
  ...t: [...infer R, (...args: any) => any]
) => any
  ? R
  : never;

type InferResults<T> = T extends (
  ...t: [...infer T, (res: infer R) => any]
) => any
  ? R
  : never;

type PromisifiedFunction<T> =
  (...args: InferArgs<T>) => Promise<InferResults<T>>;

function promisify<
  Fn extends (...args: any[]) => any,
>(
  fn: Fn
): PromisifiedFunction<Fn> {
  return function (...args: InferArgs<Fn>) {
    return new Promise((resolve) => {
      function callback(result: InferResults<Fn>) {
        resolve(result);
      };

      args.push(callback);
      fn.call(null, ...args);
    });
  }
};

declare function loadFile(
  fileName: string,
  cb: (result: string) => void,
): void;


const loadFilePromise = promisify(loadFile);

//
// Another example from the book.
//
function add(
  x: number,
  y: number,
  cb: (num: number) => void,
): void {
  cb(x + y);
}

const addAsync = promisify(add);
//
// Making an unnecessary use of ‘e => ...’ just so we can
// inspect ‘e’ its type and see it is really ‘number’.
//
addAsync(1, 1).then(e => log(e));

//
// Adding explicit type annotation.
//
const addAsync_v2: PromisifiedFunction<(
  n: number,
  m: number,
  f: (z: number) => void,
) => void> = promisify(add);

//
// Making an unnecessary use of ‘e => ...’ just so we can
// inspect its type and see it is really ‘number’.
//
addAsync_v2(1, 1).then(e => log(e));
