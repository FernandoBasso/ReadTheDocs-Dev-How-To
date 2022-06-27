export const NAME = "l32a Generic Mapped Types";

const log: Console["log"] = console.log.bind(console);

//
// ‘Keys’ is a union because of ‘keyof Obj’.
//
// USAGE:
//
//   type T = OurPick<SomeObj, "key1" | "key2">;
//
type OurPick<Obj, Keys extends keyof Obj> = {
  [Prop in Keys]: Obj[Prop];
};
//
// ‘Obj[Prop]’ is an indexed access type.
//
// ‘Prop in Keys’ is a mapped type.
//

type Jedi = {
  id: number;
  name: string;
  skills: string[];
};

type H = OurPick<Jedi, "name" | "skills">;

type VideoFormatURLs = {
  format360p: URL;
  format480p: URL;
  format720p: URL;
  format1080p: URL;
};

type HDa = OurPick<VideoFormatURLs, "format720p" | "format1080p">;
//
// Same as:
//
type HDb = {
  format720p: URL;
  format1080p: URL;
};

type OurRecord<
  Key extends string | number | symbol,
  Type,
> = {
  [Prop in Key]: Type;
};

//
// An object with one or more keys where the keys are strings
// (any strings, not specific strings) an the types of the
// values on those keys are ‘URL’.
//
var r1: OurRecord<string, URL> = {
  blog: new URL("https://fernandobasso.dev"),
  sphinx: new URL("https://devhowto.dev"),
};

r1.gitlabPages = new URL("https://fernandobasso.gitlab.io/devhowto");
