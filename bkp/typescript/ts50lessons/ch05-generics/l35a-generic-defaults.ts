export const NAME = "l35a Generic Type Defaults";

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

class Container {
  #element: Nullable<HTMLElement>;
  #prefs: UserPrefs;

  constructor(prefs: UserPrefs) {
    this.#prefs = prefs;
  }

  set element(elem: Nullable<HTMLElement>) {
    this.element = elem;
  }

  get element(): Nullable<HTMLElement> {
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

const container = new Container(defaultUserPrefs);
container.element = document.createElement("video");
container.loadVideo(videos);

//
// HTMLElement may be too generic. We may want more specific
// types so that, for example, when working with video, we
// get HTMLVideoElement methods intellisense.gg
//
