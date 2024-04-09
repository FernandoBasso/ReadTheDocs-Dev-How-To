export const NAME = "l33b Mapped Type Modifiers - Partials";

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
// No property is optional. So, we create a set of default
// user preferences.
//
const defaultP: UserPreferences = {
  format: "format1080p",
  subtitles: {
    active: false,
    language: "english",
  },
  theme: "light",
};

//
// And for users, we store deltas. That is, we store only the
// difference from the default preferences.
//

function combinePreferences(
  defaultP: UserPreferences,
  userP: UserPreferences, // <1>
): UserPreferences {
  return { ...defaultP, ...userP };
}

//
// What could be the type of ‘userP’ in <1>? If we make it
// ‘UserPreference’, it doesn't work because this object
// would generally not have a full set of preferences.
//
// User 1 just want the video format different than the
// defaults. The rest is OK for them.
//
const prefsUser1 = {
  format: "format720p",
};

combinePreferences(defaultP, prefsUser1);
//
// ‘prefsUser1’ is missing a lot of properties...
//

//
// If we make the parameter ‘userP’ be of the type ‘unknown’,
// or ‘Record<string, whatever-here>’, etc., then we cannot do
// the spread ‘{ ...defaultP, ...userP }’ because of
// incompatible types...
//
// We could create a new type where all keys are optional:
//
type OptionalUserPreferences = {
  format?: keyof VideoFormatURLs;
  subtitles?: {
    active?: boolean;
    language?: keyof SubtitleURLs;
  };
  theme?: "light" | "dark";
};
//
// Now we could use this type for the second parameter of
// ‘combinePreferences()’.
//
// But we don't want to maintain two types in this case. The
// entire structure is the same. Same keys, same types...
//
