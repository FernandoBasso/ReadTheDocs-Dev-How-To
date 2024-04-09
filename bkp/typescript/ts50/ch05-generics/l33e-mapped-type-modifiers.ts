export const NAME = "l33e Mapped Type Modifiers - Deep Modifications";

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

//
// The book is missing the ‘keyof’ token.
//
type DeepReadonly<Obj> = {
  readonly [Key in keyof Obj]: DeepReadonly<Obj[Key]>;
};

//
// Now we can't reassign it deeply. This still doesn't solve
// the runtime problem. MDN docs on Object.freeze() has a
// deepFreeze() example implementation.
//
declare let x: DeepReadonly<UserPreferences>;
x.subtitles.active = true;

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
// Only the first level of keys are made partial, but if
// we override ‘subtitles’, have to to provide its full
// sub-object or it will be missing the ‘active’ property.
//

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

//
// And a ‘DeepPartial’, just for kicks!
//
type DeepPartial<Obj> = {
  [Key in keyof Obj]?: DeepPartial<Obj[Key]>;
};
