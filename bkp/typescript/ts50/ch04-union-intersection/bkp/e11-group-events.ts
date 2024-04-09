export const NAME = "e11 group events";

const log: Console["log"] = console.log.bind(console);

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

type Conference = TechEventBase & {
  kind: "conference";
  location: string;
  price: number;
  talks: Talk[];
};

type Meetup = TechEventBase & {
  kind: "meetup";
  location: string;
  price: string;
  talks: Talk[];
};

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

type TechEvent = Conference | Meetup | Webinar | Hackaton;

//
// <1>
//
// type GroupedEvents = {
//   conference: TechEvent[],
//   meetup: TechEvent[],
//   webinar: TechEvent[],
//   hackathon: TechEvent[]
// }

type EventKind = TechEvent["kind"];

//
// Mapped type.
//
type GroupedEvents = {
  [Kind in EventKind]: TechEvent[];
};

function groupEvents(
  events: TechEvent[]
): GroupedEvents {
  //
  // <1>
  //
  const grouped = {
    conference: [],
    meetup: [],
    webinar: [],
    hackathon: []
  };

  events.forEach(event => {
    grouped[event.kind].push(event);
  });

  return grouped;
}

//
// Look at <1>! Again, should we have more or less types of
// events, we would have to update code in multiple places.
//

//
// This is how we would expand the type:
//

//
// Mapped type.
//
// 1. The original declaration.
//
type GroupedEvents1 = {
  [Kind in EventKind]: TechEvent[];
};

//
// 2. Resolve the type alias.
//
type GroupedEvents2 = {
  [Kind in TechEvent["kind"]]: TechEvent[];
};

//
// 3. Resolve the union.
//
type GroupedEvents3 = {
  [Kind in
      "conference"
    | "meetup"
    | "webinar"
    | "hackathon"
  ]: TechEvent[];
};

//
// 4. Extrapolateing the keys.
//
type GroupedEvents4 = {
  conference: TechEvent[];
  meetup: TechEvent[];
  webinar: TechEvent[];
  hackathon: TechEvent;
};

//
// Reminds me of referential transparency in lisps (scheme,
// racket, lisp).
//
