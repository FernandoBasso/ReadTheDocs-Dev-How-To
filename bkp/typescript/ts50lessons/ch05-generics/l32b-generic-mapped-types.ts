export const NAME = "l32b Generic Mapped and Indexed Access Types";

const log: Console["log"] = console.log.bind(console);

type VideoFormatURLs = {
  format360p: URL;
  format480p: URL;
  format720p: URL;
  format1080p: URL;
};

//
// With template literals, we could also do this:
//
type Vids = {
  [Key in `format${"360" | "480" | "720" | "1080"}p`]: URL;
}

type Format360 = {
  format360p: URL;
};

type Format480 = {
  format480p: URL;
};

type Format720 = {
  format720p: URL;
};

type Format1080 = {
  format1080p: URL;
};

type AvailableFormats =
    Format360
  | Format480
  | Format720
  | Format1080;

//
// With union types, we only need to fulfill the contract
// of one union constituent. Comment the line inside the
// object and the contract ceases to be fulfilled.
//
const vids: AvailableFormats = {
  // format360p: new URL("https://vimeo.com/1"),
};

//
// But more than one of the union constituents is also OK.
// It just can't have zero of the union constituents.
//
const lofi: AvailableFormats = {
  format360p: new URL("https://vimeo.com/1"),
  format480p: new URL("https://vimeo.com/1"),
}

const hq: AvailableFormats = {
  format720p: new URL("https://vimeo.com/1"),
  format1080p: new URL("https://vimeo.com/1"),
}
