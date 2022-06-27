export const NAME = "e09 filter events";

const log: Console["log"] = console.log.bind(console);

//
// INDEX ACCESS TYPES or LOOKUP TYPES
//

type Talk = {
  title: string;
  abstract: string;
  speaker: string;
};

type TechEventBase = {
  title: string;
  description: string;
  date: Date;
  capacity: number;
  rsvp: number;
};

//
// Let's use intersection types to combine the base
// type with the few other properties that change
// from type to type of event.
//

type Conference = TechEventBase & {
  kind: "conference";
  location: string;
  price: number;
  talks: Talk[];
};

//
// Pretty much the same as ‘Conference’, except that ‘price’ is
// a string ("free") instead of a number.
//
type Meetup = TechEventBase & {
  kind: "meetup";
  location: string;
  price: string;
  talks: Talk[];
};

//
// ‘Webinar’ has only one talk, and a URL instead of a
// physical location. ‘price’ is optional.
//
type Webinar = TechEventBase & {
  kind: "webinar";
  url: string;
  price?: string;
  talks: Talk; // Why plural in the book?
};

type Hackaton = TechEventBase & {
  kind: "hackathon";
  url: string;
  price?: number;
}

//
// We have a new ‘TechEvent’: ‘Hackaton’.
//
type TechEvent = Conference | Meetup | Webinar | Hackaton;

//
// But would have to also add a new kind here... Also, we
// now have to change this stuff in two places, leading to
// multiple “sources of truth”.
//
//   type EventKind = "conference" | "meetup" | "webinar" ...;
//
// But we can do this:
//
type EventKind = TechEvent["kind"];
//
// Here, we used INDEX ACCESS TYPES (sometimes a.k.a.
// LOOKUP TYPES).
//

function filterByKind(
  events: TechEvent[],
  kind: EventKind,
): TechEvent[] {
  return events.filter(event => event.kind === kind);
}

declare const events: TechEvent[];

filterByKind(events, "conference");
filterByKind(events, "webinar");
filterByKind(events, "meetup");
filterByKind(events, "hackathon");

//
// "concert" is not part of EventKind
//
filterByKind(eventList, "concert");

