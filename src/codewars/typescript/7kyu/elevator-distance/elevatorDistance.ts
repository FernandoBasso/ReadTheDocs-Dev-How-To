/**
 * Calculates the distance traveled between elevator floors.
 *
 * @param floors Array of numbers representing floors to travel to.
 * @returns The distance traveled between floors.
 */
function elevatorDistance(floors: number[]): number {
  const startingFloor: number = floors[0];

  return floors.reduce(function calculate(acc, currFloor) {
    const [distanceSoFar, floorStopped] = acc;

    const distance = Math.abs(floorStopped - currFloor);

    return [distanceSoFar + distance, currFloor];
  }, [0, startingFloor])[0];
}

export { elevatorDistance };

/*

------------------------------------------------------------------------
EXPLANATION
-----------

To calculate total the distance traveled we can simply subtract the
floor we want to travel to from the floor we are at and keep adding
those intermediate distances traveled.

Note that we care about the ABSOLUTE values because even if we go down,
say, from 5 to 2, the travel distance is the absolute value 3, not -3.

------------------------------------------------------------------------
elevatorDistance([5, 2, 8]) = 9
|5 - 2| = distance 3
|2 - 8| = distance 6
total distance: 3 + 6 = 9

------------------------------------------------------------------------
elevatorDistance([1, 2, 3]) = 2
|1 - 2| = distance 1
|2 - 3] = distance 1
total distance: 1 + 2 = 2

------------------------------------------------------------------------
elevatorDistance([7, 1, 7, 1]) = 18
|7 - 1| = distance 6
|1 - 7| = distance 6
|7 - 1] = distance 6
total distance: 6 + 6 + 6 = 18

------------------------------------------------------------------------
elevatorDistance([3, 3]) = 0
|3 - 3| = 0
total distance: 0

*/

