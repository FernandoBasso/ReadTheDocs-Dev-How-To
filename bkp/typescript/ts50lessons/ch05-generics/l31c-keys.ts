export const NAME = "l31c Working With Keys - related type parameters";

const log: Console["log"] = console.log.bind(console);

type VideoFormatURLs = {
  format360p: URL;
  format480p: URL;
  format720p: URL;
  format1080p: URL;
};

type URLObject = {
  [key: string]: URL;
};

type Loaded<Key> = {
  format: Key;
  loaded: boolean;
};

async function loadFile<
  Formats extends URLObject,
  Key extends keyof Formats,
>(
  fileFormats: Formats,
  format: Key,
): Promise<Loaded<Key>> {
  const data: Response = await fetch(fileFormats[format].href);

  return {
    format,
    loaded: data.status === 200,
  };
}

declare const video: VideoFormatURLs;

const result = await loadFile(video, "format720p");

if (result.format !== "format720p") {
  //
  // Type of format is ‘never’ here.
  //
  log(result.format);

  throw new Error("Something is wrong...");
}

