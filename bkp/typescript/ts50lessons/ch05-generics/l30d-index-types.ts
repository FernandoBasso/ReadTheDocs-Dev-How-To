export const NAME = "l30d Generic Constraints - Index Types";

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

type URLList = {
  [key: string]: URL;
};

//
// We don't know the properties themselves, but we know their
// values should be of type ‘URL’.
//
// Let's not extend from ‘object’ anymore, but from ‘URLList’.
//
function loadFile<Formats extends URLList>(
  fileFormats: Formats,
  format: string,
): void {
  if (isAvailable(fileFormats, format)) {
    log(fileFormats[format]);
  }
}
