export const MODNAME = "e03a-css";

////
// Looks like spaces are not significant here.
//
type RgbCss = `rgb(${number}, ${number},${number})`;

////
// But spaces are significant here. There must be a space
// between the comma and the next number.
//
const red: RgbCss = "rgb(255, 0, 0)";
// OK

const green: RgbCss = "rgb(0, 255)";
// NOK
// ~ Type '"rgb(0, 255)"' is not assignable to type
// ~ '`rgb(${number}, ${number}, ${number})`'
//
// We missed one numeric value.
