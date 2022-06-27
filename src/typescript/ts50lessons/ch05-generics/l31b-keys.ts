export const NAME = "l31b Working With Keys - related type parameters";

type VideoFormatURLs = {
  format360p: URL;
  format480p: URL;
  format720p: URL;
  format1080p: URL;
};

type URLObject = {
  [key: string]: URL;
};

async function loadFile<
  Formats extends URLObject,
  Key extends keyof Formats,
>(
  fileFormats: Formats,
  format: Key,
): Promise<{ format: Key, loaded: boolean }> {
  const data: Response = await fetch(fileFormats[format].href);

  return {
    format,
    loaded: data.status === 200,
  };
}

declare const video: VideoFormatURLs;

const result = loadFile(video, "format720p");
//
// This is the return type of ‘result’ now:
//
// → const result: Promise<{
// →   format: "format720p";
// →   loaded: boolean;
// → }>

