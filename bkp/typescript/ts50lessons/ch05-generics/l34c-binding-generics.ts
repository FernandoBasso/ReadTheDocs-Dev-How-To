export const NAME = "l34c Binding Generics | Generic Type Binding";

const log: Console["log"] = console.log.bind(console);

//
// The process of substituting a concrete type with a generic
// is called BINDING.
//

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

//
// Let's use a generic here now.
//
function combinePreferences<UserPrefs extends Partial<UserPreferences>>(
  defaultP: UserPreferences,
  userP: UserPrefs, // Use the generic here.
): UserPreferences & UserPrefs {
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
const prefs1 = combinePreferences(defaultUP, userPrefs);

const prefs2 = combinePreferences(
  defaultUP,
  {
    "format": "format720p",
    "theme": "dark",
  },
);

prefs2.theme;
//     "dark"
