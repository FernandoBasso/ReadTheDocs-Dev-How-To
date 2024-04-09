export const MODNAME = "e08b-locations";

type Location = "Zurich" | "Oslo" | "Beijing";

function getCountryForLocation(location: Location): string {
  switch(location) {
    case "Zurich":
      return "Switzerland";
    case "Oslo":
      return "Norway";
    default:
      throw new Error(`${location} is not known.`);
  }
}

////
// `Location` now includes Beijing.
//
getCountryForLocation("Beijing" as Location);

////
// We don't handle Kathmandu (capital of Nepal). But no TS
// errors or warnings are presented to us regarding that.
//
getCountryForLocation("Kathmandu" as any);

//
// Even though we added Beijing to Location, and added a
// default clause to the switch statement to handle unknown
// locations, the type checker did not help us see that we
// forgot to add an extra clause to the switch statement to
// handle the new location added.
//
// In other words, we forgot to handle Beijing, but got no
// type complaints. We also called getCountryForLocation()
// with Kathmandu, which we do not currently even include in
// our types, and got no type complaints either.
//
