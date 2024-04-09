export const NAME = "l23a Working with Value Types";

type EventKind = "webinar" | "conference" | "meetup";

type TechEventBase = {
  title: string;
  description: string;
  date: Date;
  capacity: number;
  rsvp: number;
  kind: EventKind; // Not ‘string’ any longer.
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

function getEventTeaser(event: TechEvent) {
  switch (event.kind) {
    case "conference":
      return `${event.title} (Conference)`;
    case "meetup":
      return `${event.title} (Meetup)`;
    case "webinar":
      return `${event.title} (Webinar)`;
    //
    // Again: cool, but not possible. "concert" is not one of
    // the possible event kinds.
    //
    case "concert":
  }
}

type TechEvent = Webinar | Conference | Meetup;

//
// Unions of value types are brilliant for control flow
// analysis. We don’t run into situations that can’t happen,
// because our types don’t support such situations. All
// possible values of the set are taken care of.
//
