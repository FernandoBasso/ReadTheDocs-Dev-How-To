export const NAME = "l35b Generic Type Defaults";

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

declare const videos: VideoFormatURLs;

type UserPrefs = {
  format: keyof VideoFormatURLs;
  subtitles: {
    active: boolean;
    language: keyof SubtitleURLs;
  };
  theme: "light" | "dark";
};

type Const<Type> = {
  readonly [Key in keyof Type]: Type[Key];
}

const defaultUserPrefs: Const<UserPrefs> = {
  format: "format1080p",
  subtitles: {
    active: false,
    language: "english",
  },
  theme: "light",
};

//
// By using both generics, we substitute for type values
// upon invocation, and we end up with intersections that
// have not values in common, therefore producing ‘never’
// for both format and theme.
//
function combinePrefs<
  Defaults extends UserPrefs,
  UserPref extends Partial<UserPrefs>,
>(
  defaultPrefs: Defaults,
  userPrefs: UserPref,
): UserPrefs {
  return {
    ...defaultPrefs,
    ...userPrefs,
  };
}

const prefs = combinePrefs(
  defaultUserPrefs,
  { format: "format1080p", theme: "dark" },
);
//
// prefs.theme.<Tab> --> no light or dark. Only
// string methods...
//

////////////////////////////////////////////////////////////////////////

type Nullable<G> = G | undefined;

//
// If we don't provide a generic explicitly, HTMLElement will,
// besides acting as the constraint, also act as the default type.
//
class Container<GenericElement extends HTMLElement> {
  #element: Nullable<GenericElement>;
  #prefs: UserPrefs;

  constructor(prefs: UserPrefs) {
    this.#prefs = prefs;
  }

  set element(elem: Nullable<GenericElement>) {
    this.element = elem;
  }

  get element(): Nullable<GenericElement> {
    return this.element;
  }

  loadVideo(formats: VideoFormatURLs) {
    const selectedFormat = formats[this.#prefs.format].href;

    if (this.#element instanceof HTMLVideoElement) {
      this.#element.src = selectedFormat;
    } else if (this.#element) {
      const vid = document.createElement("video");
      this.#element.appendChild(vid);
      vid.src = selectedFormat;
    }
  }
}

//
// Type of container is still HTMLElement.
//
const cont1 = new Container(defaultUserPrefs);
// → cont1: Container<HTMLElement>

//
// Unless we do something like:
//
const cont2 = new Container<HTMLVideoElement>(defaultUserPrefs);
// → cont2: Container<HTMLVideoElement>

//
// But we have to manually pass the generic to avoid getting
// HTMLVideoElement by default.
//

//
// NOTE: Being explicit is sometimes good, sometimes undesirable.
// I see the point, though, that here the default should really
// be HTMLVideoElement. HTMLElement should be just the constraint,
// (meaning whatever type we pass to bind the generic param, it
// should be a subtype of HTMLElement) not the default.
//
