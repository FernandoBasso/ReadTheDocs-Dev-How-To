export const MODNAME = "e08c-locations";

////
// Add a new location here and _exhaustive_ below will error
// out as expected, thus letting us know we are missing a case
// condition.
//
type Location = "Zurich" | "Oslo" | "Beijing";

function getCountryForLocation(location: Location): string {
  //
  // Comment out either of the conditions and _exhaustive_
  // below will fail to type check, helping us make sure we
  // did not forget to cover any of the constituents of
  // Location.
  //
  switch(location) {
    case "Zurich":
      return "Switzerland";
    case "Oslo":
      return "Norway";
    case "Beijing":
      return "China";
    default:
      //
      // We can only assign location to exhaustive if all
      // possible variations of location have already been
      // handled by the cases above, meaning location type was
      // narrowed down to never.
      //
      const _exhaustive_: never = location;
      throw new Error(`${location} is not known.`);
  }
}

getCountryForLocation("Beijing");
