export const NAME = "l33a Mapped Type Modifiers";

const log: Console["log"] = console.log.bind(console);

type VideoFormatURLs = {
  format360p: URL;
  format480p: URL;
  format720p: URL;
  format1080p: URL;
};

type SubtitleURLs = {
  german: URL;
  french: URL;
  english: URL;
};

type Split<Obj> = {
  [Prop in keyof Obj]: Record<Prop, Obj[Prop]>;
}[keyof Obj];

type AvailableFormats = Split<VideoFormatURLs>;

type UserPreferences = {
  format: keyof VideoFormatURLs;
  subtitles: {
    active: boolean;
    language: keyof SubtitleURLs;
  };
  theme: "light" | "dark";
};

//
// By basing ‘format’ and ‘language’ from other types, we
// avoid having to maintain extra, disconnected types.
// Updating ‘VideoFormatURLs’ and ‘SubtitleURLs’ updates
// ‘format’ and ‘language’ as well.
//
// Also, we don't allow all possible strings in the known and
// unknown universe to be a possible theme name. Oly "light"
// and "dark" strings are allowed.
//
