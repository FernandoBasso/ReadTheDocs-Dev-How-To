export const NAME = "l26b Keys and Type Predicates — keyof";

type TechEventBase = {
  title: string;
  description: string;
  date: Date;
  capacity: number;
  rsvp: number;
};

type Talk = {
  title: string;
  abstract: string;
  speaker: string;
};

type Conference = TechEventBase & {
  location: string;
  price: number;
  talks: Talk[];
  kind: "conference";
};

type Meetup = TechEventBase & {
  location: string;
  price: string;
  talks: Talk[];
  kind: "meetup";
};

type Webinar = TechEventBase & {
  url: string;
  price?: number;
  talks: Talk;
  kind: "webinar";
};

type Hackathon = TechEventBase & {
  location: string;
  price?: number;
  kind: "hackathon";
};

type TechEvent = Webinar | Conference | Meetup | Hackathon;

type EventKind = TechEvent["kind"];

//
// We are doing this...
//
type UserEvents = {
  watching: TechEvent[];
  rvsp: TechEvent[];
  attended: TechEvent[];
  signedout: TechEvent[];
};

//
// We don't do this manually:
//
//   type UserEventCategory =
//     | "watching"
//     | "rsvp"
//     | "attended"
//     | "signedoff";
//
// We create the type dynamically instead:
//
type UserEventCategory = keyof UserEvents;

function filterUserEvent(
  userEventList: UserEvents,
  category: UserEventCategory,
  filterKind?: EventKind
) {
  const filteredList = userEventList[category];

  if (filterKind) {
    return filteredList.filter((event) => event.kind === filterKind);
  }

  return filteredList;
}

declare const events: UserEvents;

//
// Now we spot the mistake!
//
filterUserEvent(events, "signedoff", "hackathon");
filterUserEvent(events, "signedout", "meetup");

//
// We can get the keys of every type.
//
type ArrayProp = keyof [];

//
// Do "f<trigger intellisense>" and see for yourself!
//
var method: ArrayProp = "filter";

type ObjProp = keyof {
  id: 1;
  name: "Yoda";
  skills: ["The Force", "Lightsaber"];
};

//
// Try to trigger intellisense!
//
var prop: ObjProp = "skills";

//
// TypeScript has this amazing idea of letting us create types
// from types and even from values.
//

