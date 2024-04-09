export const NAME = "l23c Value Types (a.k.a. Literal Types";

type TechEventBase = {
  title: string;
  description: string;
  date: Date;
  capacity: number;
  rsvp: number;
  kind: string;
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
};

type Meetup = TechEventBase & {
  location: string;
  price: string;
  talks: Talk[];
};

type Webinar = TechEventBase & {
  url: string;
  price?: number;
  talks: Talk;
};

type TechEvent = Webinar | Conference | Meetup;

type EventKind = "webinar" | "conference" | "meetup";

const tomorrowsEvent: EventKind = "show";
// → Type '"show"' is not assignable to type 'EventKind'.

//
// When we are deep in TypeScript’s type system, we do a lot
// of set widening and narrowing. Moving around in sets of
// possible values is key to define clear yet flexible types
// that give us first-class tooling.
//
