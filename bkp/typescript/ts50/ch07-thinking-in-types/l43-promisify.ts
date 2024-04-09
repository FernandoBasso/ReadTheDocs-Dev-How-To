type InferArguments<T> = T extends (
  ...t: [...infer R, (...args: any) => any]
) => any
  ? R
  : never;

type InferResults<T> = T extends (
  ...t: [...infer T, (res: infer R) => any]
) => any
  ? R
  : never;

function promisify<Fun extends (...args: any[]) => any>(
  f: Fun
): (...args: InferArguments<Fun>) => Promise<InferResults<Fun>> {
  return function (...args: InferArguments<Fun>) {
    return new Promise((resolve) => {
      function callback(result: InferResults<Fun>) {
        resolve(result);
      }
      args.push(callback);
      f.call(null, ...args);
    });
  };
}

declare function load(
  file: string,
  encoding: string,
  callback: (result: string) => void
): void;

const loadPromise = promisify(load);

loadPromise("./text.md", "utf-8").then((res) => {
  // res is string!
});

type Messages = "open" | "write" | "end" | "error";
declare function on(
  msg: Messages,
  callback: (msg: { type: Messages; content: string }) => void
): void;

const onPromise = promisify(on);

onPromise("open").then((res) => {
  console.log(res.content); // 👍
});
