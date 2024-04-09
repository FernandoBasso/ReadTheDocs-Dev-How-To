export const NAME = "l30b Generic Constraints";

const log: Console["log"] = console.log.bind(console);

type VideoFormatURLs = {
  format360p: URL;
  format480p: URL;
  format720p: URL;
  format1080p: URL;
};

type SubtitleURLs = {
  english: URL;
  german: URL;
  french: URL;
};

declare const videos: VideoFormatURLs;

declare function loadFormat(format: string): void;

//
// Let's make our generic less generic, by constraining it to
// be a subtype of ‘object’.
//
// We use the ‘extends’ keyword. Whatever ‘ObjType’ is, it
// must be a subtype (extend) of ‘object’.
//
function isAvailable<ObjType extends object>(
  obj: ObjType,
  key: string | number | symbol,
): key is keyof ObjType {
  return key in obj;
}

const format = "format240p" as string;

//
// Now our type predicate, requiring some type that is a
// subtype of object, will not accept non-object values.
//
// Author: “Even arrays are excluded.” See below.
//

if (isAvailable(null, format)) {
  // We need objects, not null...
}

if (isAvailable("Gotcha!", "length")) {
  // We need objects, not strings...
}

//
// But I am using an array here and see no problems. Which
// makes some sense because arrays are objects with numeric
// keys in ECMAScript.
//
if (isAvailable([1, 2, 3], "0")) {
  // We need objects, not arrays...
}

if (isAvailable({ name: "Yoda", skill: "The Force" }, "skill")) {
  // We have the skill key in the object provided.
}
