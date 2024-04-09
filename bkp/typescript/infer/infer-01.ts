export const name = "infer 01";

type UndefinedAsNull<T> =
  T extends undefined ? null : T;

type T1 = UndefinedAsNull<undefined>;

type T2 = UndefinedAsNull<null>;

type T3 = UndefinedAsNull<string>;

type T4 = UndefinedAsNull<null[]>;
