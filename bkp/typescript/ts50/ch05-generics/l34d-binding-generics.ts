export const NAME = "l34d Binding Generics | Generic Type Binding";

const log: Console["log"] = console.log.bind(console);

//
// Careful with too many value (literal) types. Intersection
// of value types that don't have common properties result
// in ‘never’ types.
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
// Here we use generic for both params. The return type was
// added by me (not in the book) but it just matches the
// return type that would be inferred anyway in this case.
//
function combinePreferences<
  Defaults extends UserPreferences,
  UserPrefs extends Partial<UserPreferences>
>(
  defaultP: Defaults,
  userP: UserPrefs,
): Defaults & UserPrefs {
  return { ...defaultP, ...userP };
}

const defaultUP = {
  format: "format1080p",
  subtitles: {
    active: false,
    language: "english",
  },
  theme: "light",
} as const;

const userPrefs: Partial<UserPreferences> = {
  "format": "format720p",
  "theme": "dark",
};

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

//
// No common properties in the intersection. The resulting
// type is ‘never’.
//
type T = "dark" & "light";
// → never
