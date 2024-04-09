export const NAME = "l35c Generic Type Defaults";

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
// Now we really make HTMLVideoElement the default, and
// HTMLElement only acts as the constraint.
//
class Container<GenericElement extends HTMLElement = HTMLVideoElement> {
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

const cont1 = new Container(defaultUserPrefs);
// → cont1: Container<HTMLVideoElement>

const cont2 = new Container<HTMLVideoElement>(defaultUserPrefs);
// → cont2: Container<HTMLVideoElement>

const cont3 = new Container<HTMLAnchorElement>(defaultUserPrefs);
// → cont3: Container<HTMLAnchorElement>
