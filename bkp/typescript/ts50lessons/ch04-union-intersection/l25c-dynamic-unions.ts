export const NAME = "l25c Dynamic Unions - Lookup Types";

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

//
// We create a new event type.
//
type Hackathon = TechEventBase & {
  location: string;
  price?: number;
  kind: "hackathon";
};

//
// And add ‘Hackathon’ here too!
//
type TechEvent = Webinar | Conference | Meetup | Hackathon;

//
// Instead of this:
//
//   type EventKind = "webinar" | "conference" | "meetup";
//
// We do this:
//
type EventKind = TechEvent["kind"];
//
// This is called indexed access types, or lookup types. They
// create a system of connected types. Whenever we update
// ‘TechEvent’, ‘EventKind’ is automatically updated.
//

function filterByKind(
  events: TechEvent[],
  kind: EventKind,
): TechEvent[] {
  return events.filter((el) => el.kind === kind);
}

declare const eventList: TechEvent[]

filterByKind(eventList, "hackathon");

