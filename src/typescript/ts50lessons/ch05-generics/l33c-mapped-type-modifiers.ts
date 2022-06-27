export const NAME = "l33c Mapped Type Modifiers - Partials";

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

//
// Let's create a utility type which takes a type and makes
// all its keys optional.
//
// NOTE: It handles only 1-level deep keys. Nested types are
// not touched.
//
// This utility type just replays all the keys in the original
// type, and makes them optional. Note the ‘?’!
//
type Optional<Obj> = {
  [Key in keyof Obj]?: Obj[Key];
};

type UserPreferences = {
  format: keyof VideoFormatURLs;
  subtitles: {
    active: boolean;
    language: keyof SubtitleURLs;
  };
  theme: "light" | "dark";
};

const defaultP: UserPreferences = {
  format: "format1080p",
  subtitles: {
    active: false,
    language: "english",
  },
  theme: "light",
};

function combinePreferences(
  defaultP: UserPreferences,
  userP: Optional<UserPreferences>, // <1>
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
const prefsUser1: Optional<UserPreferences> = {
  format: "format720p",
};

combinePreferences(defaultP, prefsUser1);

combinePreferences(defaultP, { theme: "dark" });

combinePreferences(defaultP, { theme: "gray" });

//
// ‘Optional’ is the built-in ‘Partial’ utility type.
//
//   type Optional<Obj> = {
//     [Key in keyof Obj]?: Obj[Key];
//   };
//
// We could also re-create its counterpart, ‘Required’.
//
type Required<Obj> = {
  [Key in keyof Obj]-?: Obj[Key];
};
//
// Note the ‘-?’ syntax!
//

//
// TypeScript already has ‘ReadOnly<Type>’ type utility!
//
type Const<Obj> = {
  readonly [Key in keyof Obj]: Obj[Key];
};

function genDefaults(obj: UserPreferences): Const<UserPreferences> {
  //
  // Object.freeze() returns Readonly<Obj>!
  //
  return Object.freeze(obj);
}

const defaultUP: Const<UserPreferences> = genDefaults({
  format: "format1080p",
  subtitles: {
    active: false,
    language: "english",
  },
  theme: "light",
});

//
// Cannot reassign it! Good!
//
defaultUP.format = "format1080p";
