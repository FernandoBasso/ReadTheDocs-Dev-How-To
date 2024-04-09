export const NAME = "l40a Composing Helper Types";

type Medium = {
  id: number;
  title: string;
  artist: string;
};

type TrackInfo = {
  tracks: number;
  duration: number;
};

type CD = Medium & TrackInfo & {
  kind: 'cd';
};

type LP = Medium & {
  sides: {
    a: TrackInfo;
    b: TrackInfo;
  },
  kind: 'lp';
};

type AllMedia = CD | LP;
type MediaKinds = AllMedia["kind"];

type SelectBranch<Branch, Kind> =
  Branch extends { kind: Kind } ? Branch : never;

type SelectLP = Extract<AllMedia, { kind: 'lp' }>;

type CDInfo_Manually_Maintained = {
  title: string;
  description: string;
  tracks: number;
  duration: number;
};

type LPInfo_Manually_Maintained = {
  title: string;
  description: string;
  sides: {
    a: {
      tracks: number;
      duration: number;
    };
    b: {
      tracks: number;
      duration: number;
    };
  };
};

type Removable = "kind" | "id";

type CDKeys = keyof CD;

//
// Same as the ‘Exclude’ built-in type utility.
//
type Remove<A, B> = A extends B ? never : A;

//
// All CD info keys, except those from ‘Removable’.
//
type CDInfoKeys = Remove<CDKeys, Removable>;

type CDInfo = Pick<
  CD,
  Exclude<keyof CD, Removable>
>;
//
// Same as:
//
type CDInfo_1 = Omit<CD, "kind" | "id">;

type LPInfo = Omit<LP, "kind" | "id">;

type RemovableKeys = "kind" | "id";
type GetInfo<Medium> = Omit<Medium, RemovableKeys>;

declare function createMedium<
  Kin extends MediaKinds
>(
  kind: Kin,
  info: GetInfo<SelectBranch<AllMedia, Kin>>
): SelectBranch<AllMedia, Kin>;

declare const cdInfo: CDInfo;
declare const lpInfo: LPInfo;

const o1 = createMedium("cd", cdInfo);
const o2 = createMedium("lp", lpInfo);

o1.tracks;
o1.sides;
o2.sides;
