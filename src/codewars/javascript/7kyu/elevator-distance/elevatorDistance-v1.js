import {
  aperture,
  apply,
  compose,
  map,
  subtract,
  sum,
} from 'ramda';

const abs = Math.abs.bind(Math);

/**
 * Calculates the distance traveled between elevator floors.
 *
 * @param {number[]} floors Array of numbers representing floors to
 *   travel to.
 * @returns The distance traveled between floors.
 */
const elevatorDistance = compose(
  sum,
  map(
    compose(
      abs,
      apply(subtract),
    ),
  ),
  aperture(2),
);

export { elevatorDistance };

/*

With aperture, we always have the stoped stationed floor as the
first elem of the tuple, and the destionation floor as the second
tuple element.

To calculate the total distance traveled from floor to floor, we need to
get the absolute value distance traveled between each of them.

So, if we are at floor 3 and go to floor 5, that is 2 floors apart.
Then, if we now move from floor 5 to floor 1, that is 4 floors apart
(not -4, we care about absolute values). And how, from floor 1 we move
to floor 7.

The above is represented with [3, 5, 1, 7]. We can, however, translate
this into a different representation that encodes the distances between
floors in a different way, using a tuple-like approach:

    [[3, 5], [5, 1], [1, 7]]

Then, we can calculate the absolute distance between the each tuple
elements:

    3 - 5 = -2
    5 - 1 = 4
    1 - 7 = -6

But remember we need the ABSOLUTE values, so, we end up with 2, 4 and 6,
which is 12 when added together.

Let's see how we could accomplish this with ramda.

    aperture([3, 5, 1, 7]);
    // → [[3, 5], [5, 1], [1, 7]]

Okay, we have the second, encoded representation. Let's subtract each
pair/tuple:

    map(apply(subtract))([[3, 5], [5, 1], [1, 7]]);
    // → [-2, 4, -6]

Hmm... We need the absolute values:

    map(compose(abs, apply(subtract)))([[5, 2], [2, 8]]);
    // → [2, 4, 6]

Great! Now we just sum the list:

    sum([2, 4, 6]);
    // → 12
*/



