export const NAME = "l31a Working With Keys - related type parameters";

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

function isAvailable<ObjType extends object>(
  obj: ObjType,
  key: string | number | symbol,
): key is keyof ObjType {
  return key in obj;
}

//
// In a non-generic world, we would use concrete types
// and be forced to create two functions.
//
function loadVideo(
  video: VideoFormatURLs,
  format: keyof VideoFormatURLs,
): void {
  // ...
}

function loadSubtitle(
  subtitles: SubtitleURLs,
  language: keyof SubtitleURLs,
): void {
  // ...
}

//
// But in a generic world, we can make do with a single
// function and some generic trickery.
//

type URLObject = {
  [key: string]: URL;
};

async function loadFile<Formats extends URLObject>(
  fileFormats: Formats,
  format: keyof Formats,
): Promise<{ format: keyof Formats, loaded: boolean }> {
  const data: Response = await fetch(fileFormats[format].href);

  return {
    format,
    loaded: data.status === 200,
  };
}
//
// NOTE: I added the return type myself. Not from the book.
//

//
// We use ‘keyof Formats’ even though format is a generic type
// parameter. We know this generic has keys because it extends
// ‘URLObject’, which has keys.
//

declare const video: VideoFormatURLs;

//
// Squiggly lines, as expected.
//
loadFile(video, "format4k");
//
// → Argument of type '"format4k"' is not assignable to
// → parameter of type 'keyof VideoFormatURLs'.
//
// ‘video’ is of the type ‘VideoFormatURLs’, so, the generic
// type parameter is made concrete to ‘VideoFormatURLs’ in
// this invocation of ‘loadFile()’. Except that
// ‘VideoFormatURLs’ does not have a "format4k" key!
//
// The second parameter to the function must be within the
// union type generated from the first param. Amazing stuff!
//

//
// This is OK, as expected.
//
const result = loadFile(video, "format720p");
//
// This is the return type of ‘result’:
//
// → const result: Promise<{
// →   format: keyof VideoFormatURLs;
// →   loaded: boolean;
// → }>
//
// Hmm. We know format is "format720p". We have narrowed it.
// Why do we still get all the possible formats in the union
// in the return type‽ We should get a more specific type.
//

