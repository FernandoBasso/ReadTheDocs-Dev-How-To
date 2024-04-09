export const NAME = "l34a Binding Generics";

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

//
// The book is missing the ‘keyof’ token.
//
type DeepReadonly<Obj> = {
  readonly [Key in keyof Obj]: DeepReadonly<Obj[Key]>;
};

//
// Because ‘Object.freeze()’ does shallow freezing, our
// ‘DeepReadonly<UserPreferences>’ return type is not entirely
// truthful, but at least we'll not be able to change deeply
// nested structures in the returned object in our own code.
//
function genDefaults(obj: UserPreferences): DeepReadonly<UserPreferences> {
  //
  // Object.freeze() returns Readonly<Obj>! But remember
  // that it does _shallow_ freezing.
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
// This works. We know the type of the object passed as the
// second parameter. It is ‘Partial<UserPreferences>’.
//
combinePreferences(
  defaultUP,
  {
    "format": "format720p",
    "theme": "dark",
  },
);

//
// But now look at this.
//
const userSettings = {
  "format": "format720p",
  "theme": "dark",
};

combinePreferences(defaultUP, userSettings);
//
// ‘userSettings’ is inferred to have this type:
//
//   const userSettings: {
//     format: string;
//     theme: string;
//   }
//
// That is, ‘userSettings’ is inferred the most reasonable
// widest possible type, which is wider than the second
// parameter of ‘combinePreferences’ allows it to be. For
// example, ‘theme’ cannot be any string in the known and
// unknown universe, neither can ‘theme’.
//

//
// But we could add CONST CONTEXT:
//
const userPrefs = {
  "format": "format720p",
  "theme": "dark",
} as const;

//
// Then the types match!
//
combinePreferences(defaultUP, userPrefs);
//
// https://blog.logrocket.com/const-assertions-are-the-killer-new-typescript-feature-b73451f35802/
//
