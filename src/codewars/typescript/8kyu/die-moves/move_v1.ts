/**
 * Return a new position.
 *
 * @param initialPos The initial position.
 * @param dieRoll The die roll.
 * @return The new position.
 */
function move(initialPos: number, dieRoll: number): number {
  return initialPos + dieRoll * 2;
}

export { move };
