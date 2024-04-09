export const NAME = "l25a Dynamic Unions";

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

type TechEvent = Webinar | Conference | Meetup;

//
// We have to recreated this union type just so we can use it
// for the ‘kind’ property in the function below.
//
// If we update even types above, or add or remove event
// types, we have to update here, and vice-versa, and we may
// have mismatches by mistake, because we now have a
// disconnection between ‘EventKind’ and the types of events.
//
type EventKind = "webinar" | "conference" | "meetup";

function filterByKind(
  events: TechEvent[],
  kind: EventKind,
): TechEvent[] {
  return events.filter((el) => el.kind === kind);
}

//
// A list of tech events we get from a back end.
//
declare const eventList: TechEvent[]

filterByKind(eventList, "conference"); // OK!
filterByKind(eventList, "webinar"); // OK!
filterByKind(eventList, "meetup"); // OK!
//
// "concert" is not part of EventKind
//
filterByKind(eventList, "concert"); // Bang!

