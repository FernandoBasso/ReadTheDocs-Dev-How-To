export const MODNAME = "e02b-board";

//
// Represent a board with N squares.
//

type ChessLetter = "A" | "B" | "C";
type ChessNumber = 1 | 2 | 3;

//
// Template literal of union types, which gives us all the permutations
// of the individual constituents.
//
type Board = `${ChessLetter}${ChessNumber}`;
// Type Board is now
// "A1" | "A2" | "A3" | "B1" | "B2" | "B3" | "C1" | "C2" | "C3"
