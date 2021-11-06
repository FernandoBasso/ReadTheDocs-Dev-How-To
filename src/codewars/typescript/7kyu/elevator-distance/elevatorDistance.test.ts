/*
To calculate total the distance traveled we can simply subtract the
floor we want to travel to from the floor we are at and keep adding
those intermediate distances traveled.

Note that we care about the ABSOLUTE values because even if we go down,
say, from 5 to 2, the travel distance is the absolute value 3, not -3.

elevatorDistance([5, 2, 8]) = 9
|5 - 2| = distance 3
|2 - 8| = distance 6
total distance: 3 + 6 = 9

elevatorDistance([1, 2, 3]) = 2
|1 - 2| = distance 1
|2 - 3] = distance 1
total distance: 1 + 2 = 2

elevatorDistance([7, 1, 7, 1]) = 18
|7 - 1| = distance 6
|1 - 7| = distance 6
|7 - 1] = distance 6
total distance: 6 + 6 + 6 = 18

elevatorDistance([3, 3]) = 0
|3 - 3| = 0
total distance: 0
*/

import { assertEquals } from "/deps.ts"
import { elevatorDistance } from "./elevatorDistance.ts"

Deno.test("elevatorDistance()", async (t) => {
  await t.step("should calculate two consecutive same floor", () => {
    assertEquals(elevatorDistance([0, 0]), 0);
    assertEquals(elevatorDistance([3, 3]), 0);
    assertEquals(elevatorDistance([17, 17]), 0);
  });

  await t.step("should calculate a few floors distance", () => {
    assertEquals(elevatorDistance([1, 2, 3]), 2);
    assertEquals(elevatorDistance([5, 2, 8]), 9);
  });

  await t.step("should calculate lengthier floor distance", () => {
    assertEquals(elevatorDistance([7, 1, 7, 1]), 18);
    assertEquals(
      elevatorDistance([7, 1, 7, 1, 18, 15, 13, 1, 9, 19, 0, 3, 7]),
      96,
    );
  });
});
