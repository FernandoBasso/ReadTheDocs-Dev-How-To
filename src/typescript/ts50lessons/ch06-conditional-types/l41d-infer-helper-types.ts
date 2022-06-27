export const NAME = "l41d infer and Helper Types";

//
// A utility type that retrieves the resolved value type of a
// promise.
//
type Unpack<T> =
  T extends Promise<infer Res> ? Res : never;

type Num = Unpack<Promise<number>>;

type Str = Unpack<Promise<string>>;

//
// A utility type that gets the type of an array's values.
//
type Flatten<T> =
  T extends Array<infer V> ? V : never;

//
// We have to use ‘Params’ with ‘typeof someFn’, not just
// ‘someFn’.
//
// Wrong:
//
//   type F = Params<f>;
//
// Correct:
//
//   type F = Params<typeof of>;
//
type Params<T> =
  T extends (...args: infer Params) => any ? Params : never;

declare function f(
  id: number,
  name: string,
  opts: Record<string, unknown>,
): void;

type F = Params<typeof f>;
