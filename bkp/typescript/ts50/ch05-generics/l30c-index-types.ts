export const NAME = "l30c Generic Constraints - Index Types";

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

//
// Observe that both types above, although having different
// keys, the value on those keys are always of the type ‘URL’.
//

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
// We don't know the properties themselves, but we know their
// values should be of type ‘URL’.
//
function loadFile<Formats extends object>(
  fileFormats: Formats,
  format: string,
): void {
  // Implementation goes here :)
}

//
// Two examples of INDEX TYPES.
//

//
// We don't know which properties ‘T1’ has, and their values
// are of unknown types.
//
type T1 = {
  [key: string]: unknown;
};

//
// We don't know the properties ‘T2’ has, but we know their
// values must be of type ‘URL’.
//
type T2 = {
  [key: string]: URL,
};

