export const MODNAME = "e04a-css";

type Gap = "margin" | "padding";
type Pos = "top" | "right" | "bottom" | "left";

type GapCss = `${Gap}-${Pos}`;
//   "margin-top" | "margin-right" | "margin-bottom"
// | "margin-left" | "padding-top" | "padding-right"
// | "padding-bottom" | "padding-left"

type Size = "rem" | "em" | "px";
type SizeCss = `${number}${Size}`;

type Spacing = {
  // Optional as we don't want to always use all the
  // properties, but a few of them, selectively as needed.
  [Key in GapCss]?: SizeCss;
};

const h: Spacing = {
  "padding-top": "2rem", // OK
  "padding-bottom": "1im", // NOK
  // ~ Type '"1im"' is not assignable to type
  // ~ `${number}rem` | `${number}em` | `${number}px`'
};
