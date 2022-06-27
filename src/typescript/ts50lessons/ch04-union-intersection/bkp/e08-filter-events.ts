export const NAME = "e08 filter events";

const log: Console["log"] = console.log.bind(console);

//
// The ‘&’ means we are making an intersection between
// ‘TechEventBase’ and the literal type on the right.
//
// The ‘|’ means we are creating a union type.
//

type Talk = {
  title: string;
  abstract: string;
  speaker: string;
};

type EventKind = "webinar" | "conference" | "meetup";

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

type TechEvent = Conference | Meetup | Webinar;

function filterByKind(
  events: TechEvent[],
  kind: EventKind,
): TechEvent[] {
  return events.filter(event => event.kind === kind);
}

declare const eventList: TechEvent[]

filterByKind(eventList, "conference");
filterByKind(eventList, "webinar");
filterByKind(eventList, "meetup");

//
// "concert" is not part of EventKind
//
filterByKind(eventList, "concert");

