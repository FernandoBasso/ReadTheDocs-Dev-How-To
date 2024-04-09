export const MODNAME = "e08a-locations";

type Location = "Zurich" | "Oslo";

function getCountryForLocation(location: Location): string {
  switch(location) {
    case "Zurich":
      return "Switzerland";
    case "Oslo":
      return "Norway";
  }
}

////
// Looks fine. But at run time, people could still pass values
// we do not understand or currently handle.
//

////
// We don't handle Beijing... But no TS errors or warnings are
// presented to us about that.
//
getCountryForLocation("Beijing" as Location);

////
// Neither do we handle Kathmandu (capital of Nepal). But again, no TS
// errors or warnings are presented to us.
//
getCountryForLocation("Kathmandu" as any);

//
// BEWARE!
//
// There is always a way to cheat the type checker in TS, and
// people will do it. A lot!
//
// We sometimes need runtime checks as well.
//
