export const NAME = "l29a Generics - Type Predicates";

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
// Because we are using the concrete type ‘VideoFormatURLs’ in
// the type predicate, which has ‘string’ keys (not numbers or
// symbols), we can type ‘key’ to be of type ‘string’.
//
function isFormatAvailable(
  obj: VideoFormatURLs,
  key: string,
): key is keyof VideoFormatURLs {
  return key in obj;
}

const format = "format240p" as string;

if (isFormatAvailable(videos, format)) {
  //
  // Inside this predicate condition, the type of ‘format’ is
  //
  //   "format360p" | "format480p" | "format720p" | "format1080p"
  //
  log(videos[format]);
}

//
// Because we are using the concrete ‘SubtitleURLs’ type in
// the type predicate, which has ‘string’ keys (not numbers or
// symbols), we can type ‘key’ to be of type ‘string’.
//
function isSubtitleAvailable(
  obj: SubtitleURLs,
  key: string,
): key is keyof SubtitleURLs {
  return key in obj
}

//
// We have two type predicate functions that do exactly the
// same thing, but for different types.
//

