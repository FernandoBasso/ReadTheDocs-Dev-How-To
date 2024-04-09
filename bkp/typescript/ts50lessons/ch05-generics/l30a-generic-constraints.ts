export const NAME = "l30a Generic Constraints";

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
// Our generic type is TOO generic.
//
function isAvailable<ObjType>(
  obj: ObjType,
  key: string | number | symbol,
): key is keyof ObjType {
  return key in obj;
}

const format = "format240p" as string;

//
// Problem is that our type predicate is TOO generic...
//

if (isAvailable(null, format)) {
  // We need objects, not null...
}

if (isAvailable("Gotcha!", "length")) {
  // We need objects, not strings...
}
