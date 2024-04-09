export const name = "l44a jsonify";

type UndefinedAsNull<T> =
  T extends undefined ? null : T;

type JSONifiedArray<T> =
  Array<UndefinedAsNull<JSONified<T>>>;

type JSONifiedObject<T> = {
  [P in keyof T]: JSONified<T[P]>;
};

type JSONifiedValue<T> =
  T extends string | number | boolean | null ? T :
  T extends Function ? never :
  T extends object ? JSONifiedObject<T> :
  T extends Array<infer U> ? JSONifiedArray<U> :
  never;

type JSONified<T> =
  JSONifiedValue<
    T extends { toJSON(): infer U } ? U : T
  >;

class Serializer<T> {
  serialize(input: T): string {
    return JSON.stringify(input);
  }

  deserialize(input: string): JSONified<T> {
    return JSON.parse(input);
  }
}

//
// ‘toJSON’ returns this object for serialization, no matter
// how many other properties this type has.
//

type Widget = {
  toJSON(): {
    kind: "Widget",
    date: Date,
  },
};

type Item = {
  //
  // Regular, primitive types.
  //
  text: string;
  count: number;

  //
  // Options get preserved.
  //
  choice: "yes" | "no" | null;

  //
  // Functions get dropped.
  //
  func: () => void;

  //
  // Nested elements need to be parsed as well.
  //
  nested: {
    isSaved: boolean;
    data: [1, undefined, 2];
  };

  //
  // A pointer to another type.
  //
  widget: Widget;

  //
  // The object referenced again.
  //
  children?: Item[];
};

const itemSerializer = new Serializer<Item>();

declare const item: Item;

const serialized = itemSerializer.serialize(item);
const obj = itemSerializer.deserialize(serialized);
