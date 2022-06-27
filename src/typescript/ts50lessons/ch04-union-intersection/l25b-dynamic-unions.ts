export const NAME = "l25b Dynamic Unions - Lookup Types";

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
// And have to remember to add ‘Hackathon’ here too, and with
// the correct casing and spelling. In real projects sometimes
// these things would be on very different modules, making it
// hard to remember or even know we would have to do it.
//
type EventKind = "webinar" | "conference" | "meetup";
//
// Oops... forgot to add "hackathon" here. 😭
//

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

//
// "hackathon" is not part of EventKind. We cannot filter by
// it even though we should be able to. 😭
//
filterByKind(eventList, "hackathon");

