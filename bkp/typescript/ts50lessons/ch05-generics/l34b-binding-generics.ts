export const NAME = "l34b Binding Generics | Type Annotations";

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

type UserPreferences = {
  format: keyof VideoFormatURLs;
  subtitles: {
    active: boolean;
    language: keyof SubtitleURLs;
  };
  theme: "light" | "dark";
};

function combinePreferences(
  defaultP: UserPreferences,
  userP: Partial<UserPreferences>,
): UserPreferences {
  return { ...defaultP, ...userP };
}

type DeepReadonly<Obj> = {
  readonly [Key in keyof Obj]: DeepReadonly<Obj[Key]>;
};

function genDefaults(obj: UserPreferences): DeepReadonly<UserPreferences> {
  return Object.freeze(obj);
}

const defaultUP: Readonly<UserPreferences> = genDefaults({
  format: "format1080p",
  subtitles: {
    active: false,
    language: "english",
  },
  theme: "light",
});

//
// Type annotation do/cause a type check the moment we assign
// a value! 💖
//
const userPrefs: Partial<UserPreferences> = {
  "format": "format720p",
  "theme": "dark",
};

//
// ‘userPrefs’ type matches now!
//
combinePreferences(defaultUP, userPrefs);
