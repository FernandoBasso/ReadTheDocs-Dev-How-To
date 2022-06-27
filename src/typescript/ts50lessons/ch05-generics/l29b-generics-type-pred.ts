export const NAME = "l29b Generics - Type Predicates";

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
// Because we are using a type predicate with a generic type
// in the type assertion, ‘key’ cannot be simply ‘string’
// because object keys in general can be numbers, strings or
// symbols as keys.
//
function isAvailable<ObjType>(
  obj: ObjType,
  key: string | number | symbol,
): key is keyof ObjType {
  return key in obj;
}

const format = "format240p" as string;

if (isAvailable(videos, format)) {
  //
  // Inside this predicate condition, the type of ‘format’ is
  //
  //   "format360p" | "format480p" | "format720p" | "format1080p"
  //
  log(videos[format]);
}

declare const subtitles: SubtitleURLs;

declare function loadSubtitle(subtitle: string): void;

const subtitle = "german" as string;

if (isAvailable(subtitles, subtitle)) {
  log(subtitles[subtitle]);
}

//
// In JavaScript, numbers and symbols are all valid key types.
// Take an array, for instance; arrays can be seen as objects
// with number keys.
//

//
// Note we can use the same ‘isAvailable()’ type predicate
// with both video formats and subtitle URLs.
//
// Also note we did not explicitly provide the type for the
// generic when invoking ‘isAvailable()’ because the type
// checker knows the types of ‘videos’ and ‘subtitles’.
//

//
// Consider:
//
function f<T>(v: T): T {
  return v;
}

f(1);
//
// Because it is clear that 1 is a number, ‘v: T’ means ‘T’
// is a number, and therefore, ‘<T>’ and the return type ‘T’
// are numbers.
//

//
// We can provide an explicit parameter for the generic type
// if we want to be explicit or need to be explicit.
//
if (isAvailable<SubtitleURLs>(subtitles, subtitle)) {
  log(subtitles[subtitle]);
};
