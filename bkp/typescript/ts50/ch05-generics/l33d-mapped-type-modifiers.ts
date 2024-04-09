export const NAME = "l33d Mapped Type Modifiers - Deep Modifications";

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
  userP: Partial<UserPreferences>,
): UserPreferences {
  return { ...defaultP, ...userP };
}

function genDefaults(obj: UserPreferences): Readonly<UserPreferences> {
  //
  // Object.freeze() returns Readonly<Obj>!
  //
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
// Only the first level of keys are made partial, but if
// we override ‘subtitles’, have to to provide its full
// sub-object or it will be missing the ‘active’ property.
//
const prefs: UserPreferences = combinePreferences(
  defaultUP,
  {
    subtitles: {
      language: "german",
      // active: false
    }
  }
);

//
// ‘Readonly’ type utility only makes the first level of
// properties readonly.
//
defaultUP.format = "format720p";

//
// The author says this will fail at runtime, but no.
// Actually, ‘Object.freeze()’ also only works on first level
// of properties.
//
// MDN docs on Object.freeze() make it clear that it does
// shallow freezing.
//
defaultUP.subtitles.language = "french";
