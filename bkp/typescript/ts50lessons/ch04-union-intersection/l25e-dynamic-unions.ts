export const NAME = "l25e Dynamic Unions - Mapped Types";

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
// This type is manually maintained and have to be kept
// in sync with ‘TechEvent’. Error prone...
//
// But note the keys are the same as the kinds on each
// type of tech event. Hmm...
//
type GroupedEvents = {
  [Kind in EventKind]: TechEvent[];
};

function groupEvents(events: TechEvent[]): GroupedEvents {
  //
  // The author should have added a type annotation here...
  //
  // I'm adding one myself. Try to comment one of the keys
  // with and without the annotation and you'll see why it
  // is important to type-annotate stuff more often than not.
  //
  // People insist thinking relying on type inference is oh
  // sooo coool! It is, except when it isn't and we fail to
  // catch mistakes...
  //
  const grouped: GroupedEvents = {
    conference: [],
    meetup: [],
    webinar: [],
    hackathon: [],
  };

  events.forEach((el) => {
    grouped[el.kind].push(el);
  });

  return grouped;
}


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

//
// Mapped types help us create a network of connected types.
//
