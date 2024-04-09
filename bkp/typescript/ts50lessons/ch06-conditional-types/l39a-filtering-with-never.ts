export const NAME = "l39a Filtering With ‘never’";

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

type SelectCD = SelectBranch<AllMedia, 'cd'>;
// Equal to
type SelectCD_1 = SelectBranch<CD | LP, 'cd'>;
//
// A conditional of unions is like a union of conditionals.
//
type SelectCD_2 =
    SelectBranch<CD, 'cd'>
  | SelectBranch<LP, 'cd'>;
//
// Substitute for the implementation.
//
type SelectCD_3 =
    (CD extends { kind: 'cd' } ? CD : never)
  | (LP extends { kind: 'cd' } ? CD : never);
//
// Evaluate.
//
type SelectCD_4 =
    //
    // This first one is true. Cool! Let's return ‘CD’.
    //
    (CD extends { kind: 'cd' } ? CD : never)
    //
    // False... Let's return ‘never’.
    //
  | (LP extends { kind: 'cd' } ? CD : never);

type SelectCD_5 = CD | never;

declare function createMedium<
  Kin extends MediaKinds,
>(
  kind: Kin,
  info: { [key: string]: unknown },
): SelectBranch<AllMedia, Kin>;

var o1 = createMedium("cd", {});
var o2 = createMedium("lp", {});

o1.sides;
o2.sides;

//
// We could do all this with the built in ‘Extract’.
//
type SelectLP = Extract<AllMedia, { kind: 'lp' }>;
