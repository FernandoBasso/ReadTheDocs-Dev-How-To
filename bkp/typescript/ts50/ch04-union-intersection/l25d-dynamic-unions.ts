export const NAME = "l25d Dynamic Unions - Mapped Types";

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
  conference: TechEvent[];
  meetup: TechEvent[];
  webinar: TechEvent[];
  hackathon: TechEvent[];
};

function groupEvents(events: TechEvent[]): GroupedEvents {
  //
  // The author should have added a type annotation here...
  //
  const grouped = {
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
