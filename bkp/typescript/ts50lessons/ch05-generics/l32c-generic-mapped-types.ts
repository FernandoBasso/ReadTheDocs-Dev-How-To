export const NAME = "l32c Generic Mapped and Indexed Access Types";

const log: Console["log"] = console.log.bind(console);

type VideoFormatURLs = {
  format360p: URL;
  format480p: URL;
  format720p: URL;
  format1080p: URL;
};

type Split1 = keyof VideoFormatURLs;
//
// Split1 is equivalent to
//
//   type Split1 =
//       "format360p"
//     | "format480p"
//     | "format720p"
//     | "format1080p";
//

type Split2 = {
  [Prop in keyof VideoFormatURLs]: Prop;
};
//
// Same as:
//
//   type Split2 = {
//     format360p: "format360p";
//     format480p: "format480p";
//     format720p: "format720p";
//     format1080p: "format1080p";
//   }
//

//
// Gets the right side of ‘Split2’ as a union.
//
type Split3 = {
  [Prop in keyof VideoFormatURLs]: Prop;
}[keyof VideoFormatURLs];
//
// Same as:
//
//   type Split3 =
//       "format360p"
//     | "format480p"
//     | "format720p"
//     | "format1080p";
//

type Split4 = {
  [Prop in keyof VideoFormatURLs]: Record<Prop, VideoFormatURLs[Prop]>;
}[keyof VideoFormatURLs];
//
// Same as:
//
// type Split4 =
//     Record<"format360p", URL>
//   | Record<"format480p", URL>
//   | Record<"format720p", URL>
//   | Record<"format1080p", URL>;
//
// And remember that
//
//   Record<"foo", "bar">
//
// is the same as
//
//  { foo: "bar" }
//

//
// Finally, we make it generic:
//
type Split<Obj> = {
  [Prop in keyof Obj]: Record<Prop, Obj[Prop]>;
}[keyof Obj];

type AvailableFormats = Split<VideoFormatURLs>;
//
// Same as:
//
// type AvailableFormats =
//     Record<"format360p", URL>
//   | Record<"format480p", URL>
//   | Record<"format720p", URL>
//   | Record<"format1080p", URL>;
//

//
// NOTE: I would probably call this utility type ‘ToUnion’
// instead of ‘Split’.
//
